export async function load(event) {
	const bunyian = await event.fetch('/Bunyian.toml');
	const tatabunyi = await event.fetch('/Tatabunyi.toml');
	return { bunyian: await bunyian.text(), tatabunyi: await tatabunyi.text() };
}
