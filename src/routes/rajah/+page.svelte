<script lang="ts">
	import init from 'wasm-rs';
	import { onMount } from 'svelte';
	import { Bunyian, RantauBunyian, KaedahBunyian, JenisJawi, parse_bunyian_toml } from 'wasm-rs';

	import * as Tabs from '$lib/components/ui/tabs/index';
	import * as PopOver from '$lib/components/ui/popover';
	import { Badge } from '$lib/components/ui/badge';
	import Table from './Table.svelte';
	import { Switch } from '$lib/components/ui/switch/index';

	export let data;
	let nasalization: Boolean = false;
	let items: Bunyian[] = [];
	let phoneticTable: Map<string, Map<string, Bunyian | undefined>>;

	onMount(async () => {
		await init(); // init initializes memory addresses needed by WASM and that will be used by JS/TS
		items = parse_bunyian_toml(data.bunyian);
		phoneticTable = data.table(items);
	});
</script>

<div class="mx-auto flex max-w-2xl flex-col place-items-center">
	<h1>Rajah Bunyi</h1>
	<Tabs.Root class="mx-auto flex max-w-7xl flex-col">
		<Tabs.List class="mx-24 justify-around">
			<Tabs.Trigger value="jawi">Jawi</Tabs.Trigger>
			<Tabs.Trigger value="rumi">Rumi</Tabs.Trigger>
			<Tabs.Trigger value="ipa">IPA</Tabs.Trigger>
		</Tabs.List>
		<div class="mx-auto flex place-content-center space-x-2 py-4">
			<Switch bind:checked={nasalization} /><span> Sengaukan </span>
			<PopOver.Root>
				<PopOver.Trigger><Badge>?</Badge></PopOver.Trigger>
				<PopOver.Content
					>Penyegauan bunyi ialah perubahan bunyi apabila dipertemukan dengan unsur bunyi sengau
					(seperti dalam imbuhan meN- dan peN-)</PopOver.Content
				>
			</PopOver.Root>
		</div>
		<Tabs.Content value="jawi">
			<Table field="jawi" {phoneticTable} {nasalization} />
		</Tabs.Content>
		<Tabs.Content value="rumi">
			<Table field="rumi" {phoneticTable} {nasalization} />
		</Tabs.Content>
		<Tabs.Content value="ipa">
			<Table field="ipa" {phoneticTable} {nasalization} />
		</Tabs.Content>
	</Tabs.Root>
</div>
