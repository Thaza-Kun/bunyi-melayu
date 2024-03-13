<script lang="ts">
    import init, { greet } from "wasm-rs";
    // we need onMount to run init
    import { onMount } from "svelte";
  
    onMount(async () => {
      await init(); // init initializes memory addresses needed by WASM and that will be used by JS/TS
    })
  </script>
  
  
  <h1>Welcome to SvelteKit</h1>
  <p>Visit <a href="https://kit.svelte.dev">kit.svelte.dev</a> to read the documentation</p>
  
  <div>
    <button on:click={() => {greet("Eloi")}}>Click Me</button>
  </div>