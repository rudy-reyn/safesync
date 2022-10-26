# object_store/init.sh

require() {
    if [ -z "$2" ]; then
        echo "Variable not set: $1" 1>&2
        exit 1
    fi
}

require MINIO_ROOT_USER "$MINIO_ROOT_USER"
require MINIO_ROOT_PASSWORD "$MINIO_ROOT_PASSWORD"

require MINIO_ACCESS_KEY "$MINIO_ACCESS_KEY"
require MINIO_SECRET_KEY "$MINIO_SECRET_KEY"
require ADDRESS "$ADDRESS"

mkdir -p objects
mkdir -p objects/staging
mkdir -p objects/partition

minio server objects --address "$ADDRESS"
