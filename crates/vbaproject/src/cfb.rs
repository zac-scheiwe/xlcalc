use crate::VbaError;

const FREESECT: u32 = 0xFFFF_FFFF;
const ENDOFCHAIN: u32 = 0xFFFF_FFFE;
const NOSTREAM: u32 = 0xFFFF_FFFF;
const MAX_REGULAR: u32 = 0xFFFF_FFFA;

fn bad(msg: &str) -> VbaError {
    VbaError::Cfb(msg.to_string())
}

fn u16_at(b: &[u8], at: usize) -> Option<u16> {
    Some(u16::from_le_bytes(b.get(at..at + 2)?.try_into().ok()?))
}

fn u32_at(b: &[u8], at: usize) -> Option<u32> {
    Some(u32::from_le_bytes(b.get(at..at + 4)?.try_into().ok()?))
}

fn words32(b: &[u8]) -> Vec<u32> {
    b.as_chunks::<4>()
        .0
        .iter()
        .map(|w| u32::from_le_bytes(*w))
        .collect()
}

pub(crate) fn words16(b: &[u8]) -> Vec<u16> {
    b.as_chunks::<2>()
        .0
        .iter()
        .map(|w| u16::from_le_bytes(*w))
        .collect()
}

struct Entry {
    name: String,
    kind: u8,
    left: u32,
    right: u32,
    child: u32,
    start: u32,
    size: u64,
}

pub struct Cfb {
    data: Vec<u8>,
    sector_size: usize,
    mini_sector_size: usize,
    mini_cutoff: u64,
    fat: Vec<u32>,
    minifat: Vec<u32>,
    entries: Vec<Entry>,
    mini_stream: Vec<u8>,
}

impl Cfb {
    pub fn new(data: Vec<u8>) -> Result<Cfb, VbaError> {
        const MAGIC: [u8; 8] = [0xD0, 0xCF, 0x11, 0xE0, 0xA1, 0xB1, 0x1A, 0xE1];
        if data.get(..8) != Some(&MAGIC[..]) {
            return Err(bad("not a compound file (bad signature)"));
        }
        let h = |at| u32_at(&data, at).ok_or_else(|| bad("truncated header"));
        let major = u16_at(&data, 0x1A).ok_or_else(|| bad("truncated header"))?;
        let shift = u16_at(&data, 0x1E).ok_or_else(|| bad("truncated header"))?;
        let mshift = u16_at(&data, 0x20).ok_or_else(|| bad("truncated header"))?;
        if !(major == 3 && shift == 9 || major == 4 && shift == 12) || mshift != 6 {
            return Err(bad("unsupported version or sector size"));
        }
        let sector_size = 1usize << shift;
        let n_fat = h(0x2C)?;
        let first_dir = h(0x30)?;
        let mini_cutoff = u64::from(h(0x38)?);
        let first_minifat = h(0x3C)?;
        let n_minifat = h(0x40)?;
        let mut difat_sector = h(0x44)?;
        let n_difat = h(0x48)?;

        let mut fat_sectors: Vec<u32> = Vec::new();
        for i in 0..109 {
            let s = h(0x4C + 4 * i)?;
            if s != FREESECT {
                fat_sectors.push(s);
            }
        }

        let mut cfb = Cfb {
            data,
            sector_size,
            mini_sector_size: 64,
            mini_cutoff,
            fat: Vec::new(),
            minifat: Vec::new(),
            entries: Vec::new(),
            mini_stream: Vec::new(),
        };

        let mut seen = 0;
        while difat_sector <= MAX_REGULAR && seen <= n_difat {
            let sec = cfb.sector(difat_sector)?.to_vec();
            let per = sector_size / 4 - 1;
            for i in 0..per {
                let s = u32_at(&sec, 4 * i).ok_or_else(|| bad("short DIFAT sector"))?;
                if s != FREESECT {
                    fat_sectors.push(s);
                }
            }
            difat_sector = u32_at(&sec, 4 * per).ok_or_else(|| bad("short DIFAT sector"))?;
            seen += 1;
        }
        fat_sectors.truncate(n_fat as usize);
        for s in fat_sectors {
            let words = words32(cfb.sector(s)?);
            cfb.fat.extend(words);
        }

        let dir = cfb.chain(first_dir, None)?;
        for rec in dir.as_chunks::<128>().0 {
            let name_len = usize::from(u16_at(rec, 0x40).unwrap_or(0)).min(64);
            let units = words16(&rec[..name_len.saturating_sub(2)]);
            cfb.entries.push(Entry {
                name: String::from_utf16_lossy(&units),
                kind: rec[0x42],
                left: u32_at(rec, 0x44).unwrap_or(NOSTREAM),
                right: u32_at(rec, 0x48).unwrap_or(NOSTREAM),
                child: u32_at(rec, 0x4C).unwrap_or(NOSTREAM),
                start: u32_at(rec, 0x74).unwrap_or(ENDOFCHAIN),
                size: u64::from(u32_at(rec, 0x78).unwrap_or(0))
                    | (u64::from(u32_at(rec, 0x7C).unwrap_or(0)) << 32),
            });
        }
        let root = cfb.entries.first().ok_or_else(|| bad("no root entry"))?;
        if root.kind != 5 {
            return Err(bad("first directory entry is not the root"));
        }
        let (root_start, root_size) = (root.start, root.size);

        if n_minifat > 0 && first_minifat <= MAX_REGULAR {
            let raw = cfb.chain(first_minifat, None)?;
            cfb.minifat = words32(&raw);
        }
        if root_start <= MAX_REGULAR {
            cfb.mini_stream = cfb.chain(root_start, Some(root_size))?;
        }
        Ok(cfb)
    }

    fn sector(&self, n: u32) -> Result<&[u8], VbaError> {
        let start = (n as usize + 1)
            .checked_mul(self.sector_size)
            .ok_or_else(|| bad("sector number overflow"))?;
        self.data
            .get(start..start + self.sector_size)
            .ok_or_else(|| bad("sector beyond end of file"))
    }

    fn chain(&self, first: u32, size: Option<u64>) -> Result<Vec<u8>, VbaError> {
        let mut out = Vec::new();
        let mut s = first;
        let mut steps = 0usize;
        while s <= MAX_REGULAR {
            if steps > self.fat.len() {
                return Err(bad("cycle in FAT chain"));
            }
            steps += 1;
            out.extend_from_slice(self.sector(s)?);
            s = *self
                .fat
                .get(s as usize)
                .ok_or_else(|| bad("FAT chain leaves the FAT"))?;
            if size.is_some_and(|n| out.len() as u64 >= n) {
                break;
            }
        }
        if let Some(n) = size {
            if (out.len() as u64) < n {
                return Err(bad("stream shorter than its size"));
            }
            out.truncate(usize::try_from(n).map_err(|_| bad("stream too large"))?);
        }
        Ok(out)
    }

    fn mini_chain(&self, first: u32, size: u64) -> Result<Vec<u8>, VbaError> {
        let mut out = Vec::new();
        let mut s = first;
        let mut steps = 0usize;
        while s <= MAX_REGULAR && (out.len() as u64) < size {
            if steps > self.minifat.len() {
                return Err(bad("cycle in mini FAT chain"));
            }
            steps += 1;
            let at = s as usize * self.mini_sector_size;
            out.extend_from_slice(
                self.mini_stream
                    .get(at..at + self.mini_sector_size)
                    .ok_or_else(|| bad("mini sector beyond the mini stream"))?,
            );
            s = *self
                .minifat
                .get(s as usize)
                .ok_or_else(|| bad("mini FAT chain leaves the mini FAT"))?;
        }
        if (out.len() as u64) < size {
            return Err(bad("mini stream shorter than its size"));
        }
        out.truncate(size as usize);
        Ok(out)
    }

    fn find_child(&self, parent: usize, name: &str) -> Option<usize> {
        let want = name.to_uppercase();
        let mut stack = vec![self.entries.get(parent)?.child];
        let mut visited = 0usize;
        while let Some(i) = stack.pop() {
            if i == NOSTREAM || visited > self.entries.len() {
                continue;
            }
            visited += 1;
            let e = self.entries.get(i as usize)?;
            if e.name.to_uppercase() == want {
                return Some(i as usize);
            }
            stack.push(e.left);
            stack.push(e.right);
        }
        None
    }

    pub fn streams(&self) -> Vec<String> {
        let mut out = Vec::new();
        let mut stack = vec![(0usize, String::new())];
        let mut guard = 0usize;
        while let Some((parent, prefix)) = stack.pop() {
            let mut sib = vec![self.entries[parent].child];
            while let Some(i) = sib.pop() {
                guard += 1;
                if i == NOSTREAM || guard > 4 * self.entries.len() + 16 {
                    continue;
                }
                let Some(e) = self.entries.get(i as usize) else {
                    continue;
                };
                let path = format!("{prefix}{}", e.name);
                match e.kind {
                    2 => out.push(path),
                    1 => stack.push((i as usize, format!("{path}/"))),
                    _ => {}
                }
                sib.push(e.left);
                sib.push(e.right);
            }
        }
        out.sort();
        out
    }

    pub fn read_stream(&self, path: &str) -> Result<Vec<u8>, VbaError> {
        let mut cur = 0usize;
        for part in path.split('/') {
            cur = self
                .find_child(cur, part)
                .ok_or_else(|| VbaError::MissingStream(path.to_string()))?;
        }
        let e = &self.entries[cur];
        if e.kind != 2 {
            return Err(VbaError::MissingStream(path.to_string()));
        }
        if e.size < self.mini_cutoff {
            self.mini_chain(e.start, e.size)
        } else {
            self.chain(e.start, Some(e.size))
        }
    }
}

const SECTOR: usize = 512;
const MINI_SECTOR: usize = 64;
const FATSECT: u32 = 0xFFFF_FFFD;

struct Node {
    name: String,

    data: Option<Vec<u8>>,
    children: Vec<usize>,
}

#[derive(Clone, Copy)]
struct Links {
    left: u32,
    right: u32,
    child: u32,
    black: bool,
}

const UNLINKED: Links = Links {
    left: NOSTREAM,
    right: NOSTREAM,
    child: NOSTREAM,
    black: true,
};

pub struct CfbBuilder {
    nodes: Vec<Node>,
}

impl Default for CfbBuilder {
    fn default() -> Self {
        Self::new()
    }
}

fn name_key(name: &str) -> (usize, Vec<u16>) {
    let units: Vec<u16> = name.to_uppercase().encode_utf16().collect();
    (units.len(), units)
}

fn link_tree(range: (usize, usize), first: u32, links: &mut [Links]) -> u32 {
    let (lo, hi) = range;
    let n = hi - lo;
    let mid = lo + n / 2;
    let id = first + mid as u32;
    let left = if mid > lo {
        link_tree((lo, mid), first, links)
    } else {
        NOSTREAM
    };
    let right = if mid + 1 < hi {
        link_tree((mid + 1, hi), first, links)
    } else {
        NOSTREAM
    };
    links[id as usize].left = left;
    links[id as usize].right = right;
    id
}

fn colour_tree(first: u32, n: usize, links: &mut [Links]) {
    let max_depth = n.ilog2();
    let full = (n + 1).is_power_of_two();
    let root = first + (n / 2) as u32;
    let mut stack = vec![(root, 0u32)];
    while let Some((id, depth)) = stack.pop() {
        let l = links[id as usize];
        links[id as usize].black = full || depth != max_depth;
        for c in [l.left, l.right] {
            if c != NOSTREAM {
                stack.push((c, depth + 1));
            }
        }
    }
}

impl CfbBuilder {
    pub fn new() -> CfbBuilder {
        CfbBuilder {
            nodes: vec![Node {
                name: "Root Entry".into(),
                data: None,
                children: Vec::new(),
            }],
        }
    }

    pub fn add_stream(&mut self, path: &str, data: Vec<u8>) {
        let mut cur = 0usize;
        let parts: Vec<&str> = path.split('/').collect();
        for (i, part) in parts.iter().enumerate() {
            let found = self.nodes[cur]
                .children
                .iter()
                .copied()
                .find(|&c| self.nodes[c].name.eq_ignore_ascii_case(part));
            cur = match found {
                Some(c) => c,
                None => {
                    self.nodes.push(Node {
                        name: (*part).to_string(),
                        data: None,
                        children: Vec::new(),
                    });
                    let id = self.nodes.len() - 1;
                    self.nodes[cur].children.push(id);
                    id
                }
            };
            if i + 1 == parts.len() {
                break;
            }
        }
        self.nodes[cur].data = Some(data);
    }

    pub fn finish(self) -> Result<Vec<u8>, VbaError> {
        let CfbBuilder { mut nodes } = self;
        for n in &nodes {
            if n.name.encode_utf16().count() > 31 {
                return Err(bad(&format!("name {} is too long", n.name)));
            }
        }

        let mut order = vec![0usize];
        let mut links = vec![UNLINKED];
        let mut queue = std::collections::VecDeque::from([0usize]);
        let mut id_of = vec![0u32; nodes.len()];
        while let Some(parent) = queue.pop_front() {
            let mut kids = std::mem::take(&mut nodes[parent].children);
            kids.sort_by_key(|&k| name_key(&nodes[k].name));
            let first = order.len() as u32;
            for &k in &kids {
                id_of[k] = order.len() as u32;
                order.push(k);
                links.push(UNLINKED);
                if nodes[k].data.is_none() {
                    queue.push_back(k);
                }
            }
            if !kids.is_empty() {
                let root = link_tree((0, kids.len()), first, &mut links);
                colour_tree(first, kids.len(), &mut links);
                links[id_of[parent] as usize].child = root;
            }
        }

        let mut fat: Vec<u32> = Vec::new();
        let mut body: Vec<u8> = Vec::new();
        let mut alloc = |data: &[u8]| -> u32 {
            if data.is_empty() {
                return ENDOFCHAIN;
            }
            let n = data.len().div_ceil(SECTOR);
            let first = fat.len() as u32;
            for i in 0..n {
                fat.push(if i + 1 == n {
                    ENDOFCHAIN
                } else {
                    first + i as u32 + 1
                });
            }
            body.extend_from_slice(data);
            body.resize(body.len().next_multiple_of(SECTOR), 0);
            first
        };
        let mut starts = vec![ENDOFCHAIN; order.len()];
        let mut mini_stream: Vec<u8> = Vec::new();
        let mut minifat: Vec<u32> = Vec::new();
        for (id, &k) in order.iter().enumerate() {
            let Some(data) = &nodes[k].data else { continue };
            if data.len() >= 4096 {
                starts[id] = alloc(data);
            } else if !data.is_empty() {
                let n = data.len().div_ceil(MINI_SECTOR);
                let first = minifat.len() as u32;
                for i in 0..n {
                    minifat.push(if i + 1 == n {
                        ENDOFCHAIN
                    } else {
                        first + i as u32 + 1
                    });
                }
                mini_stream.extend_from_slice(data);
                mini_stream.resize(mini_stream.len().next_multiple_of(MINI_SECTOR), 0);
                starts[id] = first;
            }
        }
        let mini_size = mini_stream.len() as u64;
        starts[0] = alloc(&mini_stream);
        let mut minifat_bytes: Vec<u8> = minifat.iter().flat_map(|v| v.to_le_bytes()).collect();
        minifat_bytes.resize(minifat_bytes.len().next_multiple_of(SECTOR), 0xFF);
        let minifat_start = alloc(&minifat_bytes);
        let n_minifat = (minifat_bytes.len() / SECTOR) as u32;

        let mut dir = Vec::with_capacity(order.len() * 128);
        for (id, &k) in order.iter().enumerate() {
            let n = &nodes[k];
            let units: Vec<u16> = n.name.encode_utf16().collect();
            let mut e = [0u8; 128];
            for (i, u) in units.iter().enumerate() {
                e[2 * i..2 * i + 2].copy_from_slice(&u.to_le_bytes());
            }
            e[64..66].copy_from_slice(&((units.len() as u16 + 1) * 2).to_le_bytes());
            e[66] = match (id, &n.data) {
                (0, _) => 5,
                (_, Some(_)) => 2,
                _ => 1,
            };
            let l = links[id];
            e[67] = u8::from(l.black);
            e[68..72].copy_from_slice(&l.left.to_le_bytes());
            e[72..76].copy_from_slice(&l.right.to_le_bytes());
            e[76..80].copy_from_slice(&l.child.to_le_bytes());
            let size = match (id, &n.data) {
                (0, _) => mini_size,
                (_, Some(d)) => d.len() as u64,
                _ => 0,
            };
            let start = if id == 0 || n.data.is_some() {
                starts[id]
            } else {
                0
            };
            e[116..120].copy_from_slice(&start.to_le_bytes());
            e[120..128].copy_from_slice(&size.to_le_bytes());
            dir.extend_from_slice(&e);
        }
        while dir.len() % SECTOR != 0 {
            let mut e = [0u8; 128];
            e[68..80].fill(0xFF);
            dir.extend_from_slice(&e);
        }
        let dir_start = alloc(&dir);

        let per = SECTOR / 4;
        let mut n_fat = 1;
        while (fat.len() + n_fat).div_ceil(per) > n_fat {
            n_fat += 1;
        }
        if n_fat > 109 {
            return Err(bad("project too large (over 109 FAT sectors)"));
        }
        let first_fat = fat.len() as u32;
        fat.extend(std::iter::repeat_n(FATSECT, n_fat));
        fat.resize(n_fat * per, FREESECT);

        let mut out = vec![0u8; SECTOR];
        out[..8].copy_from_slice(&[0xD0, 0xCF, 0x11, 0xE0, 0xA1, 0xB1, 0x1A, 0xE1]);
        out[0x18..0x1A].copy_from_slice(&0x3Eu16.to_le_bytes());
        out[0x1A..0x1C].copy_from_slice(&3u16.to_le_bytes());
        out[0x1C..0x1E].copy_from_slice(&0xFFFEu16.to_le_bytes());
        out[0x1E..0x20].copy_from_slice(&9u16.to_le_bytes());
        out[0x20..0x22].copy_from_slice(&6u16.to_le_bytes());
        out[0x2C..0x30].copy_from_slice(&(n_fat as u32).to_le_bytes());
        out[0x30..0x34].copy_from_slice(&dir_start.to_le_bytes());
        out[0x38..0x3C].copy_from_slice(&4096u32.to_le_bytes());
        let (mf_start, mf_n) = if minifat.is_empty() {
            (ENDOFCHAIN, 0)
        } else {
            (minifat_start, n_minifat)
        };
        out[0x3C..0x40].copy_from_slice(&mf_start.to_le_bytes());
        out[0x40..0x44].copy_from_slice(&mf_n.to_le_bytes());
        out[0x44..0x48].copy_from_slice(&ENDOFCHAIN.to_le_bytes());
        for i in 0..109 {
            let v = if i < n_fat {
                first_fat + i as u32
            } else {
                FREESECT
            };
            out[0x4C + 4 * i..0x50 + 4 * i].copy_from_slice(&v.to_le_bytes());
        }
        out.extend_from_slice(&body);
        out.extend(fat.iter().flat_map(|v| v.to_le_bytes()));
        Ok(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pattern(n: usize, seed: u8) -> Vec<u8> {
        (0..n)
            .map(|i| (i as u8).wrapping_mul(seed) ^ 0x5A)
            .collect()
    }

    #[test]
    fn round_trip_small_and_large_streams() {
        let mut b = CfbBuilder::new();
        let sizes = [0usize, 1, 63, 64, 65, 511, 512, 4095, 4096, 4097, 20_000];
        for (i, n) in sizes.iter().enumerate() {
            b.add_stream(&format!("S{i}"), pattern(*n, i as u8 + 1));
            b.add_stream(&format!("VBA/M{i}"), pattern(*n, i as u8 + 7));
        }
        let bytes = b.finish().unwrap();
        assert_eq!(bytes.len() % 512, 0);
        let cfb = Cfb::new(bytes).unwrap();
        for (i, n) in sizes.iter().enumerate() {
            assert_eq!(
                cfb.read_stream(&format!("S{i}")).unwrap(),
                pattern(*n, i as u8 + 1)
            );
            assert_eq!(
                cfb.read_stream(&format!("vba/m{i}")).unwrap(),
                pattern(*n, i as u8 + 7)
            );
        }
        assert!(cfb.read_stream("VBA/nothing").is_err());
    }

    #[test]
    fn many_siblings_make_a_valid_tree() {
        for n in [1usize, 2, 3, 4, 5, 6, 7, 8, 15, 16, 40] {
            let mut b = CfbBuilder::new();
            for i in 0..n {
                b.add_stream(&format!("Module{i}"), vec![i as u8; 10]);
            }
            let cfb = Cfb::new(b.finish().unwrap()).unwrap();
            for i in 0..n {
                assert_eq!(
                    cfb.read_stream(&format!("Module{i}")).unwrap(),
                    vec![i as u8; 10]
                );
            }
        }
    }

    #[test]
    fn long_names_are_refused() {
        let mut b = CfbBuilder::new();
        b.add_stream(&"x".repeat(32), vec![1]);
        assert!(b.finish().is_err());
    }
}
