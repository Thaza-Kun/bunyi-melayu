<script>
	import '../app.pcss';
	import { base } from '$app/paths';
</script>

<header class="sticky top-0 flex h-10 items-center gap-4 border-b bg-background px-4 md:px-6">
	<nav class="mx-auto space-x-10 sm:space-x-24">
		<a href="{base}/">Utama</a>
		<a href="{base}/tatabunyi">Tatabunyi</a>
		<a href="{base}/rajah">Rajah Bunyi</a>
	</nav>
</header>
<body>
	<div class="mx-auto flex max-w-2xl flex-col place-items-center">
		<slot />
	</div>
</body>
