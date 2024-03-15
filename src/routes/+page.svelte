<script lang="ts">
    import init, { greet } from "wasm-rs";
    // we need onMount to run init
    import { onMount } from "svelte";
    import { Bunyian, RantauBunyian, KaedahBunyian, JenisJawi, from_toml_str } from "wasm-rs";

    export let data;
    let items: Bunyian[] = [];
    onMount(async () => {
      await init(); // init initializes memory addresses needed by WASM and that will be used by JS/TS
      items = from_toml_str(data.string)
    })
</script>


<h1>Welcome to SvelteKit</h1>
<p>Visit <a href="https://kit.svelte.dev">kit.svelte.dev</a> to read the documentation</p>

{#each items as item}
  [[bunyian]]
  rantau = {RantauBunyian[item.rantau]}
  kaedah = {KaedahBunyian[item.kaedah]}
  jawi = {item.jawi}

  <!-- FOR SOME REASON, ADDING MORE ITEMS CREATE A NULL POINTER -->
  <!-- jenis_jawi = {JenisJawi[item.jenis_jawi]} -->
  <!-- rumi = {item.rumi} -->
  <!-- IPA = {item.ipa} -->
  <br/>
{/each}

<div>
    <button on:click={() => {greet("Eloi")}}>Click Me</button>
</div>