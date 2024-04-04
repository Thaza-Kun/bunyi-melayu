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
	import { Label } from '$lib/components/ui/label';
	import * as PopOver from '$lib/components/ui/popover';
	import * as RadioGroup from '$lib/components/ui/radio-group';

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
	function getParsed(t: Phonotactic, input: string, options: ParseResultOptions): ParseResults {
		return t.parse_string(input, options);
	}

	function changeValues(value: string): void {
		if (value == 'kata-nama') {
			kata_nama = true;
			kata_kerja = false;
			kata_sifat = false;
		} else if (value == 'kata-kerja') {
			kata_kerja = true;
			kata_nama = false;
			kata_sifat = false;
		} else if (value == 'kata-sifat') {
			kata_sifat = true;
			kata_nama = false;
			kata_kerja = false;
		}
	}
</script>

<h1 class="h1 text-center">Tatabunyi</h1>
<div class="m-4 flex flex-col items-center space-y-4">
	<form
		class="flex w-full max-w-sm items-center space-x-4"
		on:submit|preventDefault={() => updateText(input)}
	>
		<Input type="text" placeholder="perkataan" bind:value={input} />
		<Button type="submit" variant="secondary">Hantar</Button>
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
							<PopOver.Root>
								<PopOver.Trigger><Badge variant="destructive">!</Badge></PopOver.Trigger>
								<PopOver.Content
									>Gugusan '-{val.mid}-' tidak dikenali dalam tatabunyi {t.name.toLowerCase()}</PopOver.Content
								>
							</PopOver.Root>
							{val.head}<strong><u>{val.mid}</u></strong>{val.tail}
						{/if}
					</dd>
				{/each}
			</dl>
			<hr class="m-4" />
			<div class="grid grid-cols-3">
				<div class="col-span-2">
					{#each parse_imbuhan_toml(data.imbuhan) as i}
						{#if i.contains(kata_nama, kata_kerja, kata_sifat)}
							<Badge variant="outline">
								{i.transform_string_with(submission, parse_default_tatabunyi_toml(data.tatabunyi))}
							</Badge>
						{/if}
					{/each}
				</div>
				<div class="col-span-1">
					<h2 class="my-2">Imbuhan</h2>
					<RadioGroup.Root value="kata-nama" onValueChange={changeValues}>
						<div class="flex items-center space-x-2">
							<RadioGroup.Item value="kata-nama" id="kata-nama" />
							<Label for="kata-nama">Kata Nama</Label>
						</div>
						<div class="flex items-center space-x-2">
							<RadioGroup.Item value="kata-kerja" id="kata-kerja" />
							<Label for="kata-kerja">Kata Kerja</Label>
						</div>
						<div class="flex items-center space-x-2">
							<RadioGroup.Item value="kata-sifat" id="kata-sifat" />
							<Label for="kata-sifat">Kata Sifat</Label>
						</div>
					</RadioGroup.Root>
				</div>
			</div>
		</div>
	{/if}
</div>

<style>
</style>
