<script lang="ts">
	import init, {
		Phonotactic,
		ParseResults,
		ParseResultOptions,
		parse_tatabunyi_toml,
		parse_default_tatabunyi_toml,
		parse_imbuhan_toml
	} from 'wasm-rs';
	import { onMount } from 'svelte';

	import { Button } from '$lib/components/ui/button';
	import { Input } from '$lib/components/ui/input';
	import { Badge } from '$lib/components/ui/badge/index.js';

	export let data;
	$: kata_nama = true;
	$: kata_kerja = false;
	$: kata_sifat = false;
	$: input = '';
	$: submission = '';

	onMount(() => {
		init();
	});

	function updateText(value: string): void {
		submission = input;
	}
	import * as HoverCard from '$lib/components/ui/hover-card';
	function getParsed(t: Phonotactic, input: string, options: ParseResultOptions): ParseResults {
		return t.parse_string(input, options);
	}
</script>

<h1 class="h1 text-center">Tatabunyi</h1>
<div class="m-4 flex flex-col items-center space-y-4">
	<form
		class="flex w-full max-w-sm items-center space-x-4"
		on:submit|preventDefault={() => updateText(input)}
	>
		<Input type="text" placeholder="word" bind:value={input} />
		<Button type="submit" variant="default">Hantar</Button>
	</form>

	{#if submission != ''}
		<div class="w-full max-w-lg rounded-lg bg-white px-8 py-6 text-lg shadow-md dark:bg-gray-900">
			{submission}
			<dl>
				{#each parse_tatabunyi_toml(data.tatabunyi) as t}
					{@const val = getParsed(t, submission, new ParseResultOptions(' / '))}
					<dt>{t.name}</dt>
					<dd>
						- {#if !val.error}
							{val.full}
						{:else}
							<HoverCard.Root>
								<HoverCard.Trigger><Badge variant="destructive">!</Badge></HoverCard.Trigger>
								<HoverCard.Content
									>Gugusan '-{val.mid}-' tidak dikenali dalam tatabunyi {t.name.toLowerCase()}</HoverCard.Content
								>
							</HoverCard.Root>
							{val.head}<strong><u>{val.mid}</u></strong>{val.tail}
						{/if}
					</dd>
				{/each}
			</dl>
			<hr />
			<h2>Imbuhan</h2>
			<div>
				<label>
					<input type="checkbox" bind:checked={kata_nama} /> kata nama
				</label>
				<label>
					<input type="checkbox" bind:checked={kata_kerja} /> kata kerja
				</label>
				<label>
					<input type="checkbox" bind:checked={kata_sifat} /> kata sifat
				</label>
			</div>
			<ul>
				{#each parse_imbuhan_toml(data.imbuhan) as i}
					{#if i.contains(kata_nama, kata_kerja, kata_sifat)}
						<li>
							- {i.transform_string_with(submission, parse_default_tatabunyi_toml(data.tatabunyi))}
						</li>
					{/if}
				{/each}
			</ul>
		</div>
	{/if}
</div>

<style>
</style>
