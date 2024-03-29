import {
	Bunyian,
	RantauBunyian,
	KaedahBunyian,
	JenisJawi,
	MakhrajTajwid,
	parse_bunyian_toml
} from 'wasm-rs';
import { base } from '$app/paths';

type Enums = typeof RantauBunyian | typeof KaedahBunyian | typeof JenisJawi;
function enumKeys(items: Enums) {
	return Object.values(items).filter((v) => isNaN(Number(v)));
}

export async function load(event) {
	const bunyian = await event.fetch(`${base}/Bunyian.toml`);
	function table(items: Bunyian[]): Map<string, Map<string, Bunyian | undefined>> {
		return new Map(
			Array.from(enumKeys(RantauBunyian)).map((r) => [
				r,
				new Map(
					Array.from(
						enumKeys(KaedahBunyian).map((k) => [
							k,
							Array.from(items)
								.filter((i) => RantauBunyian[i.rantau] == r && KaedahBunyian[i.kaedah] == k)
								.shift()
						])
					)
				)
			])
		);
	}
	return { bunyian: await bunyian.text(), table };
}
