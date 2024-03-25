<script lang="ts">
	import init, { Phonotactic, parse_tatabunyi_toml } from 'wasm-rs';
	import { onMount } from 'svelte';

	import { Button } from '$lib/components/ui/button';
	import { Input } from '$lib/components/ui/input';

	export let data;
	$: input = '';
	$: submission = '';

	onMount(() => {
		init();
	});

	function updateText(value: string): void {
		submission = input;
	}
</script>

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
						▶ {t.parse_string(submission, ' / ')}
					</dd>
				{/each}
			</dl>
		</div>
	{/if}
</div>
