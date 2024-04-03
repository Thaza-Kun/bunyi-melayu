<script lang="ts">
	import init from 'wasm-rs';
	import { onMount } from 'svelte';
	import { Bunyian, RantauBunyian, KaedahBunyian, JenisJawi, parse_bunyian_toml } from 'wasm-rs';
	import * as Tabs from '$lib/components/ui/tabs/index.js';
	import Table from './Table.svelte';

	export let data;
	let items: Bunyian[] = [];
	let phoneticTable: Map<string, Map<string, Bunyian | undefined>>;
	type Enums = typeof RantauBunyian | typeof KaedahBunyian | typeof JenisJawi;

	onMount(async () => {
		await init(); // init initializes memory addresses needed by WASM and that will be used by JS/TS
		items = parse_bunyian_toml(data.bunyian);
		phoneticTable = data.table(items);
	});

	function enumKeys(items: Enums): string[] {
		return Object.values(items).filter((v) => isNaN(Number(v))) as string[];
	}

	let rantau_bunyi_names = {
		Dwibibir: 'dwibibir',
		BibirGusi: 'bibir-gusi',
		LelangitGusi: 'lelangit-gusi',
		Lelangit: 'lelangit',
		LelangitLembut: 'lelangit lembut',
		AnakTekak: 'anak tekak',
		Tekak: 'tekak'
	};
	let rantau_bunyi_shortnames = {
		Dwibibir: 'DB',
		BibirGusi: 'BG',
		LelangitGusi: 'LG',
		Lelangit: 'L',
		LelangitLembut: 'LL',
		AnakTekak: 'AT',
		Tekak: 'T'
	};
	let kaedah_bunyi_names = {
		Sengauan: 'sengauan',
		LetusanBersuara: 'letusan bersuara',
		LetusanTakBersuara: 'letusan tak bersuara',
		GeseranBersuara: 'geseran bersuara',
		GeseranTakBersuara: 'geseran tak bersuara',
		MalaranTakGeser: 'malaran tak geser',
		Getaran: 'getaran'
	};
	let kaedah_bunyi_shortnames = {
		Sengauan: 'S',
		LetusanBersuara: 'LB',
		LetusanTakBersuara: 'LXB',
		GeseranBersuara: 'GB',
		GeseranTakBersuara: 'GXB',
		MalaranTakGeser: 'MXG',
		Getaran: 'G'
	};

	function getPhoneticKey(
		table: Map<string, Map<string, Bunyian | undefined>>,
		rantau: string,
		kaedah: string
	): Bunyian | undefined {
		if (table != undefined) {
			let r = table.get(rantau);
			if (r != undefined) {
				let b = r.get(kaedah);
				return b;
			}
			return;
		}
		return;
	}
</script>

<h1 class="h1 text-center">Fonologi Moden</h1>
<Tabs.Root class="mx-auto max-w-7xl">
	<Tabs.List class="mx-auto grid max-w-sm grid-flow-col justify-stretch">
		<Tabs.Trigger value="jawi">Jawi</Tabs.Trigger>
		<Tabs.Trigger value="rumi">Rumi</Tabs.Trigger>
		<Tabs.Trigger value="ipa">IPA</Tabs.Trigger>
	</Tabs.List>
	<Tabs.Content value="jawi">
		<Table field="jawi" {phoneticTable} />
	</Tabs.Content>
	<Tabs.Content value="rumi">
		<Table field="rumi" {phoneticTable} />
	</Tabs.Content>
	<Tabs.Content value="ipa">
		<Table field="ipa" {phoneticTable} />
	</Tabs.Content>
</Tabs.Root>
