import wasmPack from 'vite-plugin-wasm-pack';

import { sveltekit } from '@sveltejs/kit/vite';
import { defineConfig } from 'vite';

export default defineConfig({
	plugins: [sveltekit(), wasmPack('./wasm-rs')]
});
