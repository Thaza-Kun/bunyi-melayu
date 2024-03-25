export async function load(event) {
    const tatabunyi = await event.fetch('/Tatabunyi.toml');
    return { tatabunyi: await tatabunyi.text() };
}
