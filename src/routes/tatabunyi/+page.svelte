<script lang="ts">
	import init, { Phonotactic, ParseResultOptions, parse_tatabunyi_toml } from 'wasm-rs';
	import { onMount } from 'svelte';

	import { Button } from '$lib/components/ui/button';
	import { Input } from '$lib/components/ui/input';
	import { Badge } from '$lib/components/ui/badge/index.js';

	export let data;
	$: input = '';
	$: submission = '';

	onMount(() => {
		init();
	});

	function updateText(value: string): void {
		submission = input;
	}
	import * as HoverCard from '$lib/components/ui/hover-card';
</script>

<HoverCard.Root>
	<HoverCard.Trigger>Hover</HoverCard.Trigger>
	<HoverCard.Content>SvelteKit - Web development, streamlined</HoverCard.Content>
</HoverCard.Root>

<h1 class="h1 text-center">Tatabunyi</h1>
<div class="m-4 flex flex-col items-center space-y-4">
	<form
		class="flex w-full max-w-sm items-center space-x-4"
		on:submit|preventDefault={() => updateText(input)}
	>
		<Input type="text" placeholder="word" bind:value={input} />
		<Button type="submit">Hantar</Button>
	</form>

	{#if submission != ''}
		<div class="w-full max-w-lg rounded-lg bg-white px-8 py-6 shadow-md dark:bg-gray-900">
			{submission}
			<dl>
				{#each parse_tatabunyi_toml(data.tatabunyi) as t}
					<dt>{t.name}</dt>
					<dd>
						<!-- FIX: Hardcode the HTML return string for now. Don't want to introduce new struct to handle for the time being -->
						▶ {@html t.parse_string(
							submission,
							new ParseResultOptions(' / ', 'u', '❔', 'parse-error')
						)}
					</dd>
				{/each}
			</dl>
		</div>
	{/if}
</div>

<style>
</style>
