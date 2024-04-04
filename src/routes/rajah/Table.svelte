<script lang="ts">
	import { Bunyian, RantauBunyian, KaedahBunyian, JenisJawi, JenisSengauan } from 'wasm-rs';
	import { Switch } from '$lib/components/ui/switch/index';

	export let nasalization: Boolean = false;
	type Enums = typeof RantauBunyian | typeof KaedahBunyian | typeof JenisJawi;

	export let field: string;
	export let phoneticTable: Map<string, Map<string, Bunyian | undefined>>;

	function enumKeys(items: Enums): string[] {
		return Object.values(items).filter((v) => isNaN(Number(v))) as string[];
	}
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
</script>

<table class="min-w-xl mx-auto table-fixed">
	<thead>
		<th scope="col"></th>
		{#each enumKeys(RantauBunyian) as rantau}
			<th scope="col" class="header"
				><span class="hidden md:block">{rantau_bunyi_names[rantau]}</span>
				<span class="visible md:hidden">{rantau_bunyi_shortnames[rantau]}</span></th
			>
		{/each}
	</thead>
	<tbody>
		{#each enumKeys(KaedahBunyian) as kaedah}
			<tr>
				<th scope="row" class="header row-head"
					><span class="hidden text-right md:block">{kaedah_bunyi_names[kaedah]}</span>
					<span class="visible md:hidden">{kaedah_bunyi_shortnames[kaedah]}</span></th
				>
				{#each enumKeys(RantauBunyian) as rantau}
					{@const bunyi = getPhoneticKey(phoneticTable, rantau, kaedah)}
					{#if bunyi != undefined}
						{#if field == 'jawi'}
							{#if nasalization}
								<td class="cell sengau-{JenisSengauan[bunyi.nasalization_type]}">
									{bunyi.nasalized(field) ? bunyi.nasalized(field) : ''}
								</td>
							{:else}
								<td class="cell jenis-{JenisJawi[bunyi.jenis_jawi]}">
									{bunyi[field]}
								</td>
							{/if}
						{:else if nasalization}
							<td class="cell sengau-{JenisSengauan[bunyi.nasalization_type]}">
								{bunyi.nasalized(field) ? bunyi.nasalized(field) : ''}
							</td>
						{:else}
							<td class="cell">
								{bunyi[field]}
							</td>
						{/if}
					{:else}
						<td class="cell-empty"></td>
					{/if}
				{/each}
			</tr>
		{/each}
	</tbody>
</table>

{#if field == 'jawi' && !nasalization}
	<div class="mx-auto my-4 flex max-w-2xl justify-evenly">
		<div class="jenis-Kongsi rounded-xl px-2">Bunyi Melayu</div>
		<div class="jenis-Arab rounded-xl px-2">Bunyi Arab</div>
		<div class="jenis-Ciptaan rounded-xl px-2">Tulisan Ciptaan</div>
	</div>
{/if}

{#if nasalization}
	<div class="mx-auto my-4 flex max-w-2xl flex-wrap justify-evenly">
		<div class="sengau-Mim rounded-xl px-2">Sengau mim</div>
		<div class="sengau-Nun rounded-xl px-2">Sengau nun</div>
		<div class="sengau-Nya rounded-xl px-2">Sengau nya</div>
		<div class="sengau-Nga rounded-xl px-2">Sengau nga</div>
		<div class="sengau-Unchanged rounded-xl px-2">Tiada perubahan</div>
	</div>
{/if}

<div
	class="visible m-4 mx-auto w-full max-w-2xl rounded-lg bg-white px-8 py-6 text-lg shadow-md dark:bg-gray-900 md:hidden"
>
	<h3 class="h3 text-center">Petunjuk</h3>
	<div class="flex justify-around">
		<div>
			<h4 class="h4">Rantau bunyi</h4>
			<ul>
				{#each enumKeys(RantauBunyian) as rantau}
					<li>
						- <strong>{rantau_bunyi_shortnames[rantau]}</strong>: {rantau_bunyi_names[rantau]}
					</li>
				{/each}
			</ul>
		</div>
		<div>
			<h4 class="h4">Kaedah bunyi</h4>
			<ul>
				{#each enumKeys(KaedahBunyian) as kaedah}
					<li>
						- <strong>{kaedah_bunyi_shortnames[kaedah]}</strong>:
						{kaedah_bunyi_names[kaedah]}
					</li>
				{/each}
			</ul>
		</div>
	</div>
</div>

<style>
	.header {
		@apply px-2 text-center md:w-20;
	}
	.row-head {
		@apply md:w-28;
	}
	.cell {
		text-align: center;
		@apply font-semibold;
		@apply bg-primary;
		@apply size-16 p-3 md:size-16;
	}
	.cell-empty {
		@apply bg-muted;
	}
	.jenis-Kongsi {
		@apply bg-primary;
	}
	.jenis-Arab {
		@apply bg-secondary;
	}
	.jenis-Ciptaan {
		@apply bg-accent;
	}
	.sengau-Mim {
		@apply bg-primary;
	}
	.sengau-Nun {
		@apply bg-secondary;
	}
	.sengau-Nya {
		@apply bg-accent;
	}
	.sengau-Nga {
		@apply bg-muted;
		@apply border-2 border-primary;
	}
	.sengau-Unchanged {
		@apply bg-muted;
		@apply border-2 border-accent;
	}
	.sengau-undefined {
		@apply cell-empty;
	}
</style>
