import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { StrictMode } from "react";
import { createRoot } from "react-dom/client";
import { ApiError } from "./api/client";
import { App } from "./app/App";
import "./index.css";

const client = new QueryClient({
  defaultOptions: {
    queries: {
      // Authentication and authorisation failures are answers, not faults.
      retry: (count, error) =>
        !(error instanceof ApiError && (error.isUnauthorized || error.isForbidden || error.status === 404)) &&
        count < 2,
      refetchOnWindowFocus: false,
    },
    mutations: { retry: false },
  },
});

const root = document.getElementById("root");
if (root === null) throw new Error("missing #root");

createRoot(root).render(
  <StrictMode>
    <QueryClientProvider client={client}>
      <App />
    </QueryClientProvider>
  </StrictMode>,
);
