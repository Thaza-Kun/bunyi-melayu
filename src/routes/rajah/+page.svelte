<script lang="ts">
	import init from 'wasm-rs';
	import { onMount } from 'svelte';
	import { Bunyian, RantauBunyian, KaedahBunyian, JenisJawi, parse_bunyian_toml } from 'wasm-rs';
	import * as Tabs from '$lib/components/ui/tabs/index.js';

	export let data;
	let items: Bunyian[] = [];
	let phoneticTable: Map<string, Map<string, Bunyian | undefined>>;
	type Enums = typeof RantauBunyian | typeof KaedahBunyian | typeof JenisJawi;

	onMount(async () => {
		await init(); // init initializes memory addresses needed by WASM and that will be used by JS/TS
		items = parse_bunyian_toml(data.bunyian);
		phoneticTable = data.table(items);
	});

	function enumKeys(items: Enums) {
		return Object.values(items).filter((v) => isNaN(Number(v)));
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
</script>

<h1 class="h1 text-center">Fonologi Moden</h1>
<Tabs.Root>
	<Tabs.List class="mx-auto grid max-w-sm grid-flow-col justify-stretch">
		<Tabs.Trigger value="jawi">Jawi</Tabs.Trigger>
		<Tabs.Trigger value="rumi">Rumi</Tabs.Trigger>
		<Tabs.Trigger value="ipa">IPA</Tabs.Trigger>
	</Tabs.List>
	<Tabs.Content value="jawi">
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
								<td class="cell jenis-{JenisJawi[bunyi.jenis_jawi]}">
									{bunyi.jawi}
								</td>
							{:else}
								<td class="cell-empty"></td>
							{/if}
						{/each}
					</tr>
				{/each}
			</tbody>
		</table>
	</Tabs.Content>
	<Tabs.Content value="rumi">
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
								<td class="cell">
									{bunyi.rumi}
								</td>
							{:else}
								<td class="cell-empty"></td>
							{/if}
						{/each}
					</tr>
				{/each}
			</tbody>
		</table>
	</Tabs.Content>
	<Tabs.Content value="ipa">
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
								<td class="cell">
									{bunyi.ipa}
								</td>
							{:else}
								<td class="cell-empty"></td>
							{/if}
						{/each}
					</tr>
				{/each}
			</tbody>
		</table>
	</Tabs.Content>
</Tabs.Root>

<style>
	.cell {
		text-align: center;
		@apply font-semibold;
		@apply bg-primary;
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
</style>
