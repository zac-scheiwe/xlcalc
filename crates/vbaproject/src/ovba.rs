use crate::VbaError;

const CHUNK: usize = 4096;

fn valid_header(src: &[u8], at: usize) -> bool {
    src.get(at..at + 2)
        .is_some_and(|h| (u16::from_le_bytes([h[0], h[1]]) >> 12) & 7 == 3)
}

fn bad(msg: &str) -> VbaError {
    VbaError::Decompress(msg.to_string())
}

pub fn decompress(src: &[u8]) -> Result<Vec<u8>, VbaError> {
    if src.first() != Some(&1) {
        return Err(bad("bad signature byte"));
    }
    let mut out: Vec<u8> = Vec::new();
    let mut pos = 1usize;
    while pos < src.len() {
        let header = src
            .get(pos..pos + 2)
            .map(|h| u16::from_le_bytes([h[0], h[1]]))
            .ok_or_else(|| bad("truncated chunk header"))?;
        if (header >> 12) & 7 != 3 {
            return Err(bad(&format!(
                "bad chunk signature {header:04x} at {pos} of {} (out {})",
                src.len(),
                out.len()
            )));
        }
        let chunk_start = out.len();
        let chunk_pos = pos;
        pos += 2;
        if header & 0x8000 == 0 {
            let end = (pos + CHUNK).min(src.len());
            out.extend_from_slice(&src[pos..end]);
            pos = end;
            continue;
        }
        let end = chunk_pos + usize::from(header & 0x0FFF) + 3;
        if end > src.len() {
            return Err(bad("chunk runs past the end"));
        }

        if end != src.len() && !valid_header(src, end) {
            return Err(bad(&format!(
                "chunk at {chunk_pos} has size {} but no chunk follows it",
                end - chunk_pos
            )));
        }
        let more = |pos: usize, out: &Vec<u8>| pos < end && out.len() - chunk_start < CHUNK;
        while more(pos, &out) {
            let flags = src[pos];
            pos += 1;
            for bit in 0..8 {
                if !more(pos, &out) {
                    break;
                }
                if flags & (1 << bit) == 0 {
                    out.push(src[pos]);
                    pos += 1;
                } else {
                    let t = src
                        .get(pos..pos + 2)
                        .map(|t| u16::from_le_bytes([t[0], t[1]]))
                        .ok_or_else(|| bad("truncated copy token"))?;
                    pos += 2;
                    let diff = out.len() - chunk_start;
                    let bits = (usize::BITS - diff.saturating_sub(1).leading_zeros()).max(4);
                    let length_mask = 0xFFFFu16 >> bits;
                    let length = usize::from(t & length_mask) + 3;
                    let offset = usize::from((t & !length_mask) >> (16 - bits)) + 1;
                    if offset > diff {
                        return Err(bad("copy token reaches before the chunk"));
                    }
                    for _ in 0..length {
                        out.push(out[out.len() - offset]);
                    }
                }
            }
        }
        pos = end;
    }
    Ok(out)
}

pub fn compress(src: &[u8]) -> Vec<u8> {
    let mut out = vec![1u8];
    if src.is_empty() {
        return out;
    }
    for chunk in src.chunks(CHUNK) {
        let packed = compress_chunk(chunk);
        match packed {
            Some(body) => {
                let header = 0xB000u16 | (body.len() as u16 + 2 - 3);
                out.extend(header.to_le_bytes());
                out.extend(body);
            }
            None => {
                out.extend(0x3FFFu16.to_le_bytes());
                out.extend(chunk);
                out.resize(out.len() + CHUNK - chunk.len(), 0);
            }
        }
    }
    out
}

fn compress_chunk(chunk: &[u8]) -> Option<Vec<u8>> {
    let mut body: Vec<u8> = Vec::new();
    let mut pos = 0usize;
    while pos < chunk.len() {
        let flag_at = body.len();
        body.push(0);
        for bit in 0..8 {
            if pos >= chunk.len() {
                break;
            }
            match longest_match(chunk, pos) {
                Some((offset, length)) => {
                    let bits = (usize::BITS - pos.saturating_sub(1).leading_zeros()).max(4);
                    let token = (((offset - 1) as u16) << (16 - bits)) | (length - 3) as u16;
                    body.extend(token.to_le_bytes());
                    body[flag_at] |= 1 << bit;
                    pos += length;
                }
                None => {
                    body.push(chunk[pos]);
                    pos += 1;
                }
            }
        }
        if body.len() > CHUNK - 2 {
            return None;
        }
    }
    (body.len() <= CHUNK - 2).then_some(body)
}

fn longest_match(chunk: &[u8], pos: usize) -> Option<(usize, usize)> {
    if pos == 0 {
        return None;
    }
    let bits = (usize::BITS - (pos - 1).leading_zeros()).max(4);
    let max_len = (0xFFFFusize >> bits) + 3;
    let max_len = max_len.min(chunk.len() - pos);
    let mut best: Option<(usize, usize)> = None;
    for start in (0..pos).rev() {
        let mut n = 0;
        while n < max_len && chunk[start + n] == chunk[pos + n] {
            n += 1;
        }
        if n >= 3 && best.is_none_or(|b| n > b.1) {
            best = Some((pos - start, n));
            if n == max_len {
                break;
            }
        }
    }
    best
}

#[cfg(test)]
mod tests {
    use super::{compress, decompress};

    fn round(data: &[u8]) {
        assert_eq!(decompress(&compress(data)).unwrap(), data);
    }

    #[test]
    fn compress_round_trips() {
        round(b"");
        round(b"a");
        round(b"abcabcabcabcabcabc");
        round(&vec![b'x'; 10_000]);
        let text: Vec<u8> = (0..30_000u32)
            .map(|i| {
                b"Sub Foo()
    x = x + 1
End Sub
"[(i * 7 % 31) as usize]
            })
            .collect();
        round(&text);

        let mut seed = 12345u32;
        let noise: Vec<u8> = (0..9000)
            .map(|_| {
                seed = seed.wrapping_mul(1_664_525).wrapping_add(1_013_904_223);
                (seed >> 24) as u8
            })
            .collect();
        let packed = compress(&noise);
        let back = decompress(&packed).unwrap();
        assert_eq!(&back[..noise.len()], &noise[..]);
    }

    #[test]
    fn compresses_a_run() {
        let data = b"abcabcabcabcabcabc";
        assert_eq!(
            compress(data),
            [0x01, 0x05, 0xB0, 0x08, 0x61, 0x62, 0x63, 0x0C, 0x20]
        );
    }

    #[test]
    fn spec_example_abc() {
        let c = [0x01, 0x05, 0xB0, 0x08, 0x61, 0x62, 0x63, 0x0C, 0x20];
        assert_eq!(decompress(&c).unwrap(), b"abcabcabcabcabcabc");
    }

    #[test]
    fn two_chunks_reset_the_offset_width() {
        let mut c = vec![0x01, 0xFF, 0x3F];
        c.extend(std::iter::repeat_n(b'x', 4096));
        c.extend([0x05, 0xB0, 0x08, 0x61, 0x62, 0x63, 0x0C, 0x20]);
        let mut want = vec![b'x'; 4096];
        want.extend(b"abcabcabcabcabcabc");
        assert_eq!(decompress(&c).unwrap(), want);
    }

    #[test]
    fn wide_offsets() {
        let lits: Vec<u8> = (b'a'..b'a' + 20).collect();
        let mut body = vec![0x00];
        body.extend(&lits[..8]);
        body.push(0x00);
        body.extend(&lits[8..16]);
        body.push(0x10);
        body.extend(&lits[16..20]);
        let token: u16 = 18 << 11;
        body.extend(token.to_le_bytes());
        let header = 0xB000u16 | (body.len() as u16 - 1);
        let mut c = vec![0x01];
        c.extend(header.to_le_bytes());
        c.extend(body);
        let mut want = lits.clone();
        want.extend(&lits[1..4]);
        assert_eq!(decompress(&c).unwrap(), want);
    }

    #[test]
    fn raw_chunk() {
        let mut c = vec![0x01, 0x02, 0x30, b'h', b'i', b'!'];
        assert_eq!(decompress(&c).unwrap(), b"hi!");
        c[0] = 2;
        assert!(decompress(&c).is_err());
    }

    #[test]
    fn rejects_bad_input() {
        assert!(decompress(&[]).is_err());
        assert!(decompress(&[1, 0x05]).is_err());
        assert!(decompress(&[1, 0x05, 0xB0, 0x08]).is_err());

        assert!(decompress(&[1, 0x02, 0xB0, 0x01, 0x00, 0x00]).is_err());
    }
}
