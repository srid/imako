import { Component, onMount, Show } from "solid-js";
import { HashRouter, Route } from "@solidjs/router";
import { isConnected } from "@/store";
import { connectVault } from "@/sync/websocket";
import { initTheme } from "@/state/theme";
import { Header } from "@/components/Header";
import VaultPage from "@/pages/VaultPage";

const Layout: Component<{ children?: any }> = (props) => {
  return (
    <div class="max-w-6xl mx-auto my-8 px-4">
      <div class="bg-white dark:bg-stone-950 rounded-2xl shadow-sm border border-stone-200 dark:border-stone-700 p-6 sm:p-8">
        <Header />
        <main>
          {props.children}
        </main>
      </div>
    </div>
  );
};

const App: Component = () => {
  onMount(() => {
    initTheme();
    connectVault();
  });

  return (
    <div class="min-h-screen bg-accent-50/30 dark:bg-stone-900 text-stone-800 dark:text-stone-200">
      <Show
        when={isConnected()}
        fallback={
          <div class="flex items-center justify-center min-h-screen">
            <div class="text-center">
              <div class="inline-block w-8 h-8 border-4 border-accent-500 border-t-transparent rounded-full animate-spin mb-4" />
              <p class="text-stone-500 dark:text-stone-400">Connecting to vault...</p>
            </div>
          </div>
        }
      >
        <HashRouter root={Layout}>
          <Route path="/" component={VaultPage} />
          <Route path="/p/*vaultPath" component={VaultPage} />
        </HashRouter>
      </Show>
    </div>
  );
};

export default App;
