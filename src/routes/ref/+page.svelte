<script lang="ts">
	import init from 'wasm-rs';
	// we need onMount to run init
	import { onMount } from 'svelte';
	import {
		Bunyian,
		Phonotactic,
		RantauBunyian,
		KaedahBunyian,
		JenisJawi,
		MakhrajTajwid,
		parse_bunyian_toml,
		parse_tatabunyi_toml
	} from 'wasm-rs';

	export let data;
	let items: Bunyian[] = [];
	let tatabunyi: Phonotactic[] = [];
	type PTable = Map<string, Map<string, Bunyian | undefined>>;
	type MTable = Map<string, Bunyian[]>;
	let phoneticTable: PTable;
	let makhrajTable: MTable;
	type Enums = typeof RantauBunyian | typeof KaedahBunyian | typeof JenisJawi;

	onMount(async () => {
		await init(); // init initializes memory addresses needed by WASM and that will be used by JS/TS
		items = parse_bunyian_toml(data.bunyian);
		tatabunyi = parse_tatabunyi_toml(data.tatabunyi);
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
		makhrajTable = new Map(
			Array.from(enumKeys(MakhrajTajwid)).map((m) => [
				m,
				Array.from(items).filter((i) => MakhrajTajwid[i.makhraj] == m)
			])
		);
	});

	function getPhoneticKey(table: PTable, rantau: string, kaedah: string): Bunyian | undefined {
		if (table != undefined) {
			let r = table.get(rantau);
			if (r != undefined) {
				let b = r.get(kaedah);
				return b;
			}
		}
		return;
	}
	function getMakhrajKey(table: MTable, makhraj: string): Bunyian[] | undefined {
		if (table != undefined) {
			console.log(table.get(makhraj));
			return table.get(makhraj);
		}
		return;
	}

	function enumKeys(items: Enums) {
		return Object.values(items).filter((v) => isNaN(Number(v)));
	}
</script>

<h2>Tatabunyi (Phonotactic)</h2>
<li>
	{#each tatabunyi as t}
		menyanyi ➡ {t.parse_string('menyanyi')}
	{/each}
</li>

<h2>Fonologi Moden</h2>
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
					{@const bunyi = getPhoneticKey(phoneticTable, rantau, kaedah)}
					{#if bunyi != undefined}
						<td
							style="text-align: center;"
							class="makhraj-{MakhrajTajwid[bunyi.makhraj]} jenis-{JenisJawi[bunyi.jenis_jawi]}"
							>{bunyi.jawi}</td
						>
					{:else}
						<td style="background-color: black;"></td>
					{/if}
				{/each}
			</tr>
		{/each}
	</tbody>
</table>

<h2>Hukum Tajwid</h2>
<table class="table">
	<thead>
		<th scope="col"></th>
		{#each enumKeys(MakhrajTajwid) as makhraj}
			<th scope="col">{makhraj}</th>
		{/each}
	</thead>
	<tbody>
		<tr>
			<th scope="row">{' '}</th>
			{#each enumKeys(MakhrajTajwid) as makhraj}
				{@const bunyi = getMakhrajKey(makhrajTable, makhraj)}
				{#if bunyi != undefined}
					<td>{bunyi.map((i) => i.jawi)}</td>
				{:else}
					<td style="background-color: black;"></td>
				{/if}
			{/each}
		</tr>
	</tbody>
</table>

<style>
	/* .makhraj-PangkalHalkum {
		border-width: 2px;
		border-color: rebeccapurple;
	}
	.makhraj-TengahHalkum {
		border-width: 2px;
		border-color: red;
	}
	.makhraj-HujungHalkum {
		border-width: 2px;
		border-color: blue;
	}
	.makhraj-PangkalLidah {
		border-width: 2px;
		border-color: gold;
	}
	.makhraj-TengahLidah {
		border-width: 2px;
		border-color: orange;
	}
	.makhraj-TepiLidah {
		border-width: 2px;
		border-color: olivedrab;
	}
	.makhraj-HujungLidah {
		border-width: 2px;
		border-color: green;
	}
	.makhraj-DuaBibir {
		border-width: 2px;
		border-color: turquoise;
	} */
	.jenis-Kongsi {
		background-color: yellow;
	}
	.jenis-Ciptaan {
		background-color: cyan;
	}
	.jenis-Arab {
		background-color: orange;
	}
</style>
