<script lang="ts">
	import init, { Phonotactic, parse_tatabunyi_toml } from 'wasm-rs';
	import { onMount } from 'svelte';

	import { Button } from '$lib/components/ui/button';
	import { Input } from '$lib/components/ui/input';

	export let data;
	$: input = '';

	onMount(() => {
		init();
	});
</script>

<form class="flex w-full max-w-sm items-center space-x-2">
	<Input type="text" placeholder="word" bind:value={input} />
	<!-- <Button type="submit">Submit</Button> -->
</form>

{#if input != ''}
	{input}
	<dl>
		{#each parse_tatabunyi_toml(data.tatabunyi) as t}
			<!--BUG: Error only show the parsed string. Should tell user that it is error -->
			<dt>{t.name}</dt>
			<dd>
				▶ {t.parse_string(input, ' / ')}
			</dd>
		{/each}
	</dl>
{/if}
