import toml_to_bunyians from "wasm-rs";

export async function load(event) {
    const data = await event.fetch('/Bunyian.toml')
    return {string: await data.text()}
}