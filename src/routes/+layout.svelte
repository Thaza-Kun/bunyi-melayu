<script>
	import '../app.pcss';
</script>

<header class="sticky top-0 flex h-10 items-center gap-4 border-b bg-background px-4 md:px-6">
	<nav>
		<a href="/">Utama</a>
		<a href="/tatabunyi">Tatabunyi</a>
		<a href="/rajah">Rajah Bunyi</a>
	</nav>
</header>
<body>
	<slot />
</body>
