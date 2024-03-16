<script lang="ts">
	import init from 'wasm-rs';
	// we need onMount to run init
	import { onMount } from 'svelte';
	import { Bunyian, RantauBunyian, KaedahBunyian, JenisJawi, from_toml_str } from 'wasm-rs';

	export let data;
	let items: Bunyian[] = [];
	type Table = Map<string, Map<string, Bunyian | undefined>>;
	let phoneticTable: Table;
	type Enums = typeof RantauBunyian | typeof KaedahBunyian | typeof JenisJawi;
	onMount(async () => {
		await init(); // init initializes memory addresses needed by WASM and that will be used by JS/TS
		items = from_toml_str(data.string);
		phoneticTable = new Map(
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
	});

	function getPhoneticKey(table: Table, rantau: string, kaedah: string): string {
		if (table != undefined) {
			let r = table.get(rantau);
			if (r != undefined) {
				let b = r.get(kaedah);
				if (b != undefined) {
					return b.jawi;
				} else {
					return '';
				}
			}
		}
		return '';
	}

	function enumKeys(items: Enums) {
		return Object.values(items).filter((v) => isNaN(Number(v)));
	}
</script>

<h1>Welcome to SvelteKit</h1>
<p>Visit <a href="https://kit.svelte.dev">kit.svelte.dev</a> to read the documentation</p>
<table class="table">
	<thead>
		<th scope="col"></th>
		{#each enumKeys(RantauBunyian) as rantau}
			<th scope="col">{rantau}</th>
		{/each}
	</thead>
	<tbody>
		{#each enumKeys(KaedahBunyian) as kaedah}
			<tr>
				<th scope="row">{kaedah}</th>
				{#each enumKeys(RantauBunyian) as rantau}
					<td>
						{getPhoneticKey(phoneticTable, rantau, kaedah)}
					</td>
				{/each}
			</tr>
		{/each}
	</tbody>
</table>
