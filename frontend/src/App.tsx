import { Component, onMount, Show } from "solid-js";
import { Router, Route, Navigate } from "@solidjs/router";
import { vault, isConnected } from "@/store";
import { connectVault } from "@/sync/websocket";
import { NavBar } from "@/components/NavBar";
import TasksPage from "@/pages/TasksPage";
import NotesPage from "@/pages/NotesPage";

const Layout: Component<{ children?: any }> = (props) => {
  return (
    <div class="max-w-4xl mx-auto my-6">
      {/* Vault path label */}
      <div class="text-center">
        <span class="inline-block px-3 py-1 text-xs font-mono bg-indigo-600 text-white rounded-t-lg">
          {vault.vaultPath}
        </span>
      </div>

      {/* Main content card */}
      <div class="bg-white dark:bg-gray-950 rounded-xl shadow-sm border border-indigo-600 p-6 sm:p-8 -mt-px">
        <NavBar />
        {props.children}
      </div>
    </div>
  );
};

const App: Component = () => {
  onMount(() => {
    connectVault();
  });

  return (
    <div class="min-h-screen bg-gray-50 dark:bg-gray-900 font-sans text-gray-900 dark:text-gray-100">
      <Show
        when={isConnected()}
        fallback={
          <div class="flex items-center justify-center min-h-screen">
            <div class="text-center">
              <div class="inline-block w-8 h-8 border-4 border-indigo-600 border-t-transparent rounded-full animate-spin mb-4" />
              <p class="text-gray-500 dark:text-gray-400">Connecting to vault...</p>
            </div>
          </div>
        }
      >
        <Router root={Layout}>
          <Route path="/" component={() => <Navigate href="/tasks" />} />
          <Route path="/tasks" component={TasksPage} />
          <Route path="/notes" component={NotesPage} />
        </Router>
      </Show>
    </div>
  );
};

export default App;
