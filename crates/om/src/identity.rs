use std::collections::HashMap;
use xlcalc::{ColumnId, SheetId, SheetKind, TableId};
use xlcalc_variant::{ObjectId, Variant};

use crate::app::{Application, BookId, Object, RangeRef};
use crate::controls::Coll;
use crate::pivot::PivotRef;
use crate::shapes::DrawKind;

#[derive(Default)]
pub(crate) struct Identity {
    owners: HashMap<ObjectId, ObjectId>,

    aliases: HashMap<ObjectId, ObjectId>,

    fore_colors: HashMap<ObjectId, (BookId, SheetId, u32)>,
}

impl Identity {
    pub(crate) fn forget(&mut self, id: ObjectId) {
        self.owners.remove(&id);
        self.aliases.remove(&id);
        self.fore_colors.remove(&id);
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Key {
    Application,

    Book(BookId),

    Sheet(BookId, SheetId),

    Name(BookId, Option<SheetId>, String, u32),

    Table(BookId, TableId),

    Column(BookId, TableId, ColumnId),

    PivotCache(BookId, usize),

    Shapes(BookId, SheetId),

    ControlShape(BookId, SheetId, u32),

    Drawing(BookId, SheetId, u32, &'static str),

    Handle(ObjectId),
}

impl Application<'_> {
    pub(crate) fn same_object(&self, a: ObjectId, b: ObjectId) -> bool {
        a == b || self.identity_key(a) == self.identity_key(b)
    }

    pub(crate) fn identity_key(&self, id: ObjectId) -> Key {
        if let Some(&origin) = self.identity.aliases.get(&id) {
            return Key::Handle(origin);
        }
        let own = Key::Handle(id);
        if let Some(&(b, s, shape)) = self.identity.fore_colors.get(&id) {
            return Key::Drawing(b, s, shape, "ForeColor");
        }
        let Some(object) = self.arena.get(id) else {
            return own;
        };
        match object {
            Object::Application => Key::Application,
            Object::Workbook(b) => Key::Book(*b),
            Object::Worksheet(b, s) => {
                let kind = self
                    .book(*b)
                    .ok()
                    .and_then(|bk| bk.wb.sheet(*s))
                    .map(|x| x.kind);
                if matches!(kind, Some(SheetKind::Macro | SheetKind::MacroIntl)) {
                    own
                } else {
                    Key::Sheet(*b, *s)
                }
            }
            Object::Chart(b, s) => Key::Sheet(*b, *s),
            Object::Name(b, s, k, serial) => Key::Name(*b, *s, k.clone(), *serial),
            Object::Table(b, t, true) => Key::Table(*b, *t),
            Object::ListColumn(b, t, c, true) => Key::Column(*b, *t, *c),
            Object::Pivot(PivotRef::Cache(b, i)) => Key::PivotCache(*b, *i),
            Object::Controls(b, s, Coll::Shapes) => Key::Shapes(*b, *s),
            Object::Control(c) if c.shape => Key::ControlShape(c.book, c.sheet, c.uid),
            Object::Draw(d) => {
                let part = match d.kind {
                    DrawKind::Shapes => return Key::Shapes(d.book, d.sheet),
                    DrawKind::Shape => "Shape",
                    DrawKind::Chart => "Chart",
                    DrawKind::Fill => "Fill",
                    DrawKind::LineFmt => "Line",
                    DrawKind::TextFrame2 => "TextFrame2",
                    DrawKind::Stub("ShadowFormat") => "Shadow",
                    _ => return own,
                };
                Key::Drawing(d.book, d.sheet, d.id, part)
            }
            _ => own,
        }
    }

    pub(crate) fn part_parent(&mut self, part: ObjectId) -> Option<Variant> {
        let owner = *self.identity.owners.get(&part)?;
        let made = match self.arena.get(owner)?.clone() {
            Object::Range(r) => Object::Range(self.retrack(r)?),
            Object::Multi(rs) => Object::Multi(
                rs.into_iter()
                    .map(|r| self.retrack(r))
                    .collect::<Option<_>>()?,
            ),
            style @ Object::Style(..) => style,
            _ => return None,
        };
        let id = self.arena.insert(made);
        self.identity.aliases.insert(id, owner);
        Some(Variant::Dispatch(Some(id)))
    }

    fn retrack(&mut self, r: RangeRef) -> Option<RangeRef> {
        let (sheet, rect) = self.resolve(&r).ok()?;
        let made = self.track_ref(r.book, sheet, rect, r.mode).ok()?;
        Some(RangeRef {
            axis: r.axis,
            ..made
        })
    }

    pub(crate) fn note_part(&mut self, from: ObjectId, member: &str, result: &Variant) {
        let Variant::Dispatch(Some(to)) = result else {
            return;
        };
        let to = *to;
        let (Some(source), Some(made)) = (self.arena.get(from), self.arena.get(to)) else {
            return;
        };
        if let Object::Draw(d) = source {
            if matches!(d.kind, DrawKind::Fill) && member.eq_ignore_ascii_case("ForeColor") {
                let shape = (d.book, d.sheet, d.id);
                self.identity.fore_colors.insert(to, shape);
            }
            return;
        }
        let part = matches!(
            made,
            Object::Font(_)
                | Object::Interior(_)
                | Object::Borders(_)
                | Object::Border(..)
                | Object::Validation(_)
                | Object::ValidationMulti(_)
                | Object::Areas(_)
                | Object::Characters(..)
                | Object::FormatConditions(_)
                | Object::FormatConditionsMulti(_)
                | Object::DisplayFormat(_)
        );
        if !part {
            return;
        }
        let owner = match source {
            Object::Range(_) | Object::Multi(_) | Object::Style(..) => {
                Some(*self.identity.aliases.get(&from).unwrap_or(&from))
            }

            Object::Borders(_) => self.identity.owners.get(&from).copied(),
            _ => None,
        };
        if let Some(owner) = owner {
            self.identity.owners.insert(to, owner);
        }
    }
}
