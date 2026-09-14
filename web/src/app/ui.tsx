import type { ReactNode } from "react";
import { ApiError } from "../api/client";

export const Spinner = ({ label }: { label: string }) => (
  <div className="flex items-center gap-3 py-8 text-sm text-neutral-400">
    <span className="size-4 animate-spin rounded-full border-2 border-neutral-600 border-t-neutral-200" />
    {label}
  </div>
);

/**
 * Renders a failure with its server-supplied message. Chilin returns a precise
 * diagnostic for every rejection, so showing it beats a generic apology.
 */
export const ErrorNotice = ({ error, action }: { error: unknown; action?: ReactNode }) => {
  const status = error instanceof ApiError ? error.status : null;
  const message = error instanceof Error ? error.message : String(error);
  return (
    <div className="rounded-lg border border-red-900/60 bg-red-950/40 px-4 py-3 text-sm">
      <div className="flex items-start justify-between gap-4">
        <div>
          <span className="font-medium text-red-300">
            {status === null ? "Request failed" : `Error ${status}`}
          </span>
          <p className="mt-1 text-red-200/80">{message}</p>
        </div>
        {action}
      </div>
    </div>
  );
};

export const Panel = ({
  title,
  description,
  children,
  actions,
}: {
  title: string;
  description?: string;
  children: ReactNode;
  actions?: ReactNode;
}) => (
  <section className="rounded-xl border border-neutral-800 bg-neutral-900/50">
    <header className="flex items-start justify-between gap-4 border-b border-neutral-800 px-5 py-4">
      <div>
        <h2 className="text-sm font-semibold text-neutral-100">{title}</h2>
        {description !== undefined && <p className="mt-0.5 text-xs text-neutral-500">{description}</p>}
      </div>
      {actions}
    </header>
    <div className="px-5 py-4">{children}</div>
  </section>
);

export const Button = ({
  children,
  onClick,
  type = "button",
  variant = "default",
  disabled = false,
}: {
  children: ReactNode;
  onClick?: () => void;
  type?: "button" | "submit";
  variant?: "default" | "primary" | "danger";
  disabled?: boolean;
}) => {
  const palette = {
    default: "border-neutral-700 bg-neutral-800 text-neutral-200 hover:bg-neutral-700",
    primary: "border-sky-700 bg-sky-800 text-sky-50 hover:bg-sky-700",
    danger: "border-red-900 bg-red-950 text-red-200 hover:bg-red-900",
  }[variant];
  return (
    <button
      type={type}
      onClick={onClick}
      disabled={disabled}
      className={`rounded-md border px-3 py-1.5 text-xs font-medium transition disabled:cursor-not-allowed disabled:opacity-40 ${palette}`}
    >
      {children}
    </button>
  );
};

export const Field = ({
  label,
  hint,
  children,
}: {
  label: string;
  hint?: string;
  children: ReactNode;
}) => (
  <label className="block">
    <span className="text-xs font-medium text-neutral-300">{label}</span>
    {hint !== undefined && <span className="ml-2 text-xs text-neutral-600">{hint}</span>}
    <div className="mt-1.5">{children}</div>
  </label>
);

export const inputClass =
  "w-full rounded-md border border-neutral-700 bg-neutral-950 px-3 py-1.5 text-sm text-neutral-100 outline-none placeholder:text-neutral-600 focus:border-sky-700";

export const Empty = ({ children }: { children: ReactNode }) => (
  <p className="py-6 text-center text-sm text-neutral-500">{children}</p>
);
