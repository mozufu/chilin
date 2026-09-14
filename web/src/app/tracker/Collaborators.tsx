import { useState } from "react";
import { usePermissionMutation, usePermissions, type Permission } from "../../api/queries";
import { Button, Empty, ErrorNotice, Field, Panel, Spinner, inputClass } from "../ui";

const LEVELS: Permission["access"][] = ["read", "write", "admin"];

export const Collaborators = ({
  owner,
  name,
  canAdminister,
}: {
  owner: string;
  name: string;
  canAdminister: boolean;
}) => {
  const permissions = usePermissions(owner, name, canAdminister);
  const change = usePermissionMutation(owner, name);
  const [user, setUser] = useState("");
  const [access, setAccess] = useState<Permission["access"]>("write");

  if (!canAdminister) {
    return (
      <Panel title="Collaborators">
        <Empty>Administrator access is required to manage collaborators.</Empty>
      </Panel>
    );
  }

  return (
    <div className="space-y-4">
      <Panel title="Collaborators" description="Who can read, write, and administer this repository">
        {permissions.isPending && <Spinner label="Loading collaborators" />}
        {permissions.error !== null && <ErrorNotice error={permissions.error} />}
        {permissions.data !== undefined && (
          <ul className="divide-y divide-neutral-800">
            {permissions.data.map((entry) => {
              // The owner's administration is structural rather than granted,
              // so it has no row to revoke and no level to change.
              const isOwner = entry.user === owner;
              return (
                <li key={entry.user} className="flex items-center justify-between gap-4 py-2.5">
                  <div className="min-w-0">
                    <p className="truncate text-sm text-neutral-200">{entry.user}</p>
                    <p className="text-xs text-neutral-600">
                      {entry.access}
                      {isOwner && <span className="ml-1.5 text-amber-600">owner</span>}
                    </p>
                  </div>
                  {!isOwner && (
                    <div className="flex items-center gap-1.5">
                      {LEVELS.map((level) => (
                        <button
                          key={level}
                          type="button"
                          disabled={change.isPending || level === entry.access}
                          onClick={() => change.mutate({ user: entry.user, access: level })}
                          className={`rounded-full px-2 py-0.5 text-xs transition disabled:opacity-30 ${
                            level === entry.access
                              ? "bg-neutral-200 text-neutral-900"
                              : "bg-neutral-800 text-neutral-400 hover:text-neutral-200"
                          }`}
                        >
                          {level}
                        </button>
                      ))}
                      <Button
                        variant="danger"
                        disabled={change.isPending}
                        onClick={() => change.mutate({ user: entry.user, access: null })}
                      >
                        Remove
                      </Button>
                    </div>
                  )}
                </li>
              );
            })}
          </ul>
        )}
        {change.error !== null && (
          <div className="mt-3">
            <ErrorNotice error={change.error} />
          </div>
        )}
      </Panel>

      <Panel title="Add a collaborator" description="The user must already exist in the registry">
        <form
          className="space-y-3"
          onSubmit={(event) => {
            event.preventDefault();
            change.mutate({ user: user.trim(), access }, { onSuccess: () => setUser("") });
          }}
        >
          <Field label="User">
            <input
              className={inputClass}
              value={user}
              onChange={(event) => setUser(event.target.value)}
              placeholder="bob"
            />
          </Field>
          <div className="flex gap-1.5">
            {LEVELS.map((level) => (
              <button
                key={level}
                type="button"
                onClick={() => setAccess(level)}
                className={`rounded-full px-2.5 py-0.5 text-xs transition ${
                  access === level
                    ? "bg-neutral-200 text-neutral-900"
                    : "bg-neutral-800 text-neutral-400 hover:text-neutral-200"
                }`}
              >
                {level}
              </button>
            ))}
          </div>
          <Button type="submit" variant="primary" disabled={change.isPending || user.trim().length === 0}>
            {change.isPending ? "Saving…" : "Grant access"}
          </Button>
        </form>
      </Panel>
    </div>
  );
};
