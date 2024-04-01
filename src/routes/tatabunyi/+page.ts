import { base } from '$app/paths';

export async function load(event) {
	const tatabunyi = await event.fetch(`${base}/Tatabunyi.toml`);
	const imbuhan = await event.fetch(`${base}/Imbuhan.toml`);
	return { tatabunyi: await tatabunyi.text(), imbuhan: await imbuhan.text() };
}
