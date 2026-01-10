use gpui_markup::ui;

fn main() {
    // Should fail: empty expression in parentheses
    let _ = ui! { () };
}
