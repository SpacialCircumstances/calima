use std::cell::RefCell;
use std::fmt::{Display, Formatter};

pub trait FormatWithContext<'a> {
    type Context;

    fn format(&'a self, vtc: &mut Self::Context, f: &mut Formatter<'_>) -> std::fmt::Result;
}

pub fn format_ctx_iter<
    'a,
    C,
    T: FormatWithContext<'a, Context = C> + 'a,
    I: Iterator<Item = &'a T>,
>(
    ctx: &mut C,
    f: &mut Formatter<'_>,
    mut iter: I,
    sep: &str,
) -> std::fmt::Result {
    let mut next = iter.next();

    while let Some(ni) = next.take() {
        ni.format(ctx, f)?;
        next = iter.next();

        if let Some(_) = &next {
            write!(f, "{}", sep)?;
        }
    }

    Ok(())
}

pub fn format_ctx_iter_end<
    'a,
    C,
    T: FormatWithContext<'a, Context = C> + 'a,
    I: Iterator<Item = &'a T>,
>(
    ctx: &mut C,
    f: &mut Formatter<'_>,
    mut iter: I,
    sep: &str,
) -> std::fmt::Result {
    for it in iter {
        it.format(ctx, f)?;
        write!(f, "{}", sep)?;
    }

    Ok(())
}

struct WithCtx<'a, C, T: FormatWithContext<'a, Context = C>>(&'a T, RefCell<&'a mut C>);

impl<'a, C, T: FormatWithContext<'a, Context = C>> Display for WithCtx<'a, C, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.format(*self.1.borrow_mut(), f)
    }
}

pub fn format_to_string<'a, C, T: FormatWithContext<'a, Context = C>>(
    t: &'a T,
    ctx: &'a mut C,
) -> String {
    WithCtx(t, RefCell::new(ctx)).to_string()
}
