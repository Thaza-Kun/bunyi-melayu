<script lang="ts">
	import { base } from '$app/paths';
</script>

<h1>
	<ruby class="flex flex-col">
		<rt class="text-center font-mono text-2xl">&laquo; Bunyian Melayu &raquo;</rt>
		<span>📝<span class="font-mono text-6xl">بوڽيان ملايو</span> 💬</span>
	</ruby>
</h1>

<h2>PERIHAL</h2>

<div class="note">
	<div class="flex items-center space-x-3">
		<svg class="mr-2 h-4 w-4 fill-current" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20"
			><path
				d="M12.432 0c1.34 0 2.01.912 2.01 1.957 0 1.305-1.164 2.512-2.679 2.512-1.269 0-2.009-.75-1.974-1.99C9.789 1.436 10.67 0 12.432 0zM8.309 20c-1.058 0-1.833-.652-1.093-3.524l1.214-5.092c.211-.814.246-1.141 0-1.141-.317 0-1.689.562-2.502 1.117l-.528-.88c2.572-2.186 5.531-3.467 6.801-3.467 1.057 0 1.233 1.273.705 3.23l-1.391 5.352c-.246.945-.141 1.271.106 1.271.317 0 1.357-.392 2.379-1.207l.6.814C12.098 19.02 9.365 20 8.309 20z"
			/></svg
		>
		<p>
			Ini merupakan projek sampingan untuk menambah kefungsian aplikasi perkamusan 🌊 <a
				href="https://github.com/samudradev/samudra-app">Samudra</a
			> 🌊.
		</p>
	</div>
</div>

<div>
	<p>
		Bunyian Melayu merupakan projek untuk menerapkan sistem <a href="{base}/tatabunyi#perihal"
			>tatabunyi</a
		> bahasa secara beraturcara. Sebagai sistem untuk melengkapi perisian perkamusan, projek ini bermanfaat
		dalam skop-skop berikut:
	</p>
	<ol>
		<li>
			<span class="font-bold">Ejaan</span>
			<ul>
				<li>Perkataan yang didaftarkan dalam kamus boleh disemak menepati sistem ejaan.</li>
				<li>Ejaan istilah baharu boleh ditentusahkan tanpa bergantung pada data kata sedia ada.</li>
				<li>Suku kata perkataan boleh dicerakinkan secara automatik.</li>
				<li>
					Perkataan-perkataan asing (yang tidak mematuhi sistem ejaan bahasa Melayu) dapat ditapis
					dari ayat yang rojak.
				</li>
				<li>Perkataan yang sama boleh ditukar-tukar cara ejaan mengikut sistem ejaan berbeza.</li>
			</ul>
		</li>
		<li>
			<span class="font-bold">Perubahan Bunyi</span>
			<ul>
				<li>
					Ada masanya sebuah bunyi berubah sifat berdasarkan bunyi sebelum atau selepasnya.
					Contohnya, imbuhan meN- dan peN- mengubah suku kata awal perkataan selepasnya melalui
					penyegauan. Penyegauan tersebut boleh dilihat di <a href="{base}/rajah">rajah bunyi</a>.
				</li>
			</ul>
		</li>
		<li>
			<span class="font-bold">Pembentukan Kata</span>
			<ul>
				<li>
					Perkataan terbitan seperti imbuhan dan kata ganda boleh dihasilkan berdasarkan petua suku
					kata dan perubahan bunyi.
				</li>
			</ul>
		</li>
	</ol>
</div>

<style>
</style>
