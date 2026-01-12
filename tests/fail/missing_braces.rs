use gpui_markup::ui;

fn main() {
    let _ = ui! {
        div
    };
    let _ = ui! {
        div {
            div
        }
    };
    let _ = ui! {
        div @[]
    };
    let _ = ui! {
        Header
    };
    let _ = ui! {
        div {
            Header
        }
    };
}
