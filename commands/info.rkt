#lang info

(define raco-commands
  '(("db-migrate" ratemig/commands/migrate "migrate db by migration files." 100)
    ("db-rollback" ratemig/commands/rollback "rollback db by migration files" 50)
    ("gen-migration" ratemig/commands/gen "generate migration file." 50)))
