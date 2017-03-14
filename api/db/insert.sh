#/bin/bash
read -p "This script may overwrite existing data, are you sure you wisth to procced? (y/n)" proceed
if [[ "$proceed" != "y" ]]; then
    echo "aborting..."
    exit
fi

# Move to the correct directory
cd $(dirname "${BASH_SOURCE[0]}")

# Lookup the database user's 
user=${DATABASE_USER:-snow_public_user}
password=${DATABASE_USER:-beammeup}
database=${DATABASE_NAME:-snow_public}

echo "Running schema"
psql -U $user -d $database -f "./schema.sql"

## Executes and builds the schema.
env=${API_ENV:-Development}

if [[ $env=Development ]]; then
    echo "inserting development data"
    psql -U $user -d $database -f "./devdata.sql"
fi

echo "Done"
