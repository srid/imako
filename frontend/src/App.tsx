import { Component, onMount, Show } from "solid-js";
import { Router, Route, Navigate } from "@solidjs/router";
import { isConnected } from "@/store";
import { connectVault } from "@/sync/websocket";
import { Header } from "@/components/Header";
import TasksPage from "@/pages/TasksPage";
import NotesPage from "@/pages/NotesPage";

const Layout: Component<{ children?: any }> = (props) => {
  return (
    <div class="max-w-4xl mx-auto my-8 px-4">
      <div class="bg-white dark:bg-stone-950 rounded-2xl shadow-sm border border-stone-200 dark:border-stone-700 p-6 sm:p-8">
        <Header />
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
