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

<h1>
	<ruby class="flex flex-col">
		<rt class="text-center font-mono text-2xl">&laquo; Tatabunyi &raquo;</rt>
		<span>📝 <span class="font-mono text-6xl">تاتابوڽي</span> 📰</span>
	</ruby>
</h1>

<form
	class="flex w-full max-w-sm items-center space-x-4"
	on:submit|preventDefault={() => updateText(input)}
>
	<Input type="text" placeholder="perkataan" bind:value={input} />
	<Button type="submit" variant="secondary">Hantar</Button>
</form>

{#if submission == ''}
	<h2 id="perihal" class="m-4">PERIHAL</h2>
	<p>
		<a href="https://en.wikipedia.org/wiki/Phonotactics">Tatabunyi bahasa</a> merupakan kaedah
		pembunyian suku kata dalam sesuatu bahasa. Setiap bahasa ada bentuk suku kata yang dibenarkan
		dan ada bentuk suku kata yang tidak dibenarkan. Contohnya, dalam bahasa Inggeris, bunyi
		/ŋ/(dieja: <i>ng</i>) boleh muncul di hujung suku kata (seperti <i>sing</i>) tetapi tidak boleh
		muncul di awal suku kata. Hal ini yang menyebabkan mereka membunyikan 'dengan' sebagai '<i
			>the gun</i
		>'.
	</p>
	<div class="p-3"></div>
{/if}

{#if submission != ''}
	<!-- <div class="m-4 flex flex-col items-center space-y-4"> -->
	<div class="m-4 w-full max-w-lg rounded-lg bg-white px-8 py-6 text-lg shadow-md dark:bg-gray-900">
		<h2 class="underline">
			{submission}
		</h2>
		<dl>
			{#each parse_tatabunyi_toml(data.tatabunyi) as t}
				{@const val = getParsed(t, submission, new ParseResultOptions(' / '))}
				<dt>{t.name}</dt>
				<dd>
					- {#if !val.error}
						{@html val.full}
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
	<!-- </div> -->
{/if}
<div class="note">
	<h3>Cubalah!</h3>
	<p>Pengaruh bahasa lain boleh mengubah tatabunyi bahasa. Cuba beberapa perkataan ini:</p>
	<ul>
		<li>musytari</li>
		<li>masyarakat</li>
		<li>varian</li>
	</ul>
	<p>
		Lihat bagaimana bahasa Melayu lama, bahasa Melayu klasik dan bahasa Melayu moden membunyikannya.
	</p>
</div>

<style>
</style>
