import { base } from '$app/paths';

export async function load(event) {
	const tatabunyi = await event.fetch(`${base}/Tatabunyi.toml`);
	return { tatabunyi: await tatabunyi.text() };
}
