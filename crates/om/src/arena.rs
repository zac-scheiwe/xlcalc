use xlcalc_variant::ObjectId;

pub(crate) struct Arena<T> {
    slots: Vec<Slot<T>>,
    free: Vec<u32>,
}

struct Slot<T> {
    generation: u32,
    value: Option<T>,
}

fn pack(generation: u32, index: u32) -> ObjectId {
    ObjectId(((generation as u64) << 32) | index as u64)
}

impl<T> Arena<T> {
    pub fn new() -> Arena<T> {
        Arena {
            slots: Vec::new(),
            free: Vec::new(),
        }
    }

    pub fn insert(&mut self, value: T) -> ObjectId {
        match self.free.pop() {
            Some(i) => {
                let slot = &mut self.slots[i as usize];
                slot.value = Some(value);
                pack(slot.generation, i)
            }
            None => {
                self.slots.push(Slot {
                    generation: 1,
                    value: Some(value),
                });
                pack(1, self.slots.len() as u32 - 1)
            }
        }
    }

    pub fn get(&self, id: ObjectId) -> Option<&T> {
        let slot = self.slots.get(id.0 as u32 as usize)?;
        if slot.generation == (id.0 >> 32) as u32 {
            slot.value.as_ref()
        } else {
            None
        }
    }

    pub fn get_mut(&mut self, id: ObjectId) -> Option<&mut T> {
        let slot = self.slots.get_mut(id.0 as u32 as usize)?;
        if slot.generation == (id.0 >> 32) as u32 {
            slot.value.as_mut()
        } else {
            None
        }
    }

    pub fn remove(&mut self, id: ObjectId) -> Option<T> {
        let index = id.0 as u32;
        let slot = self.slots.get_mut(index as usize)?;
        if slot.generation != (id.0 >> 32) as u32 {
            return None;
        }
        let value = slot.value.take()?;
        slot.generation = slot.generation.wrapping_add(1).max(1);
        self.free.push(index);
        Some(value)
    }
}

impl<T> Arena<T> {
    pub fn for_each_mut(&mut self, mut f: impl FnMut(&mut T)) {
        for slot in &mut self.slots {
            if let Some(v) = slot.value.as_mut() {
                f(v);
            }
        }
    }
}
