The backend utilizes a private REST api called by the client application for managing files and
partitions. The synchronization api is defined is with the following endpoints:

| Endpoint | Methods |
| --- | --- |
| /download  | GET |
| /preview | GET |
| /upload | POST |
| /replace | POST, PUT |
| /remove | POST, DELETE |

A `/partitions` endpoint also exists and redirects to one of the other endpoints depending on
method and parameters:

| Method | Redirects To | Params |
| --- | --- | --- |
| GET | /download  | null |
| GET | /preview | preview=true |
| POST | /upload | null |
| PUT | /replace | null |
| DELETE | /remove | null |

## Endpoint Usages

---

**`/download`, `/preview`, `/delete`:**

```json
{
	"user_id": "<user_id>",
	"partition_ids": ["<partition_id>", ...]
}
```

**`/upload` , `/replace`:**

```json
{
	"user_id": "<user_id>",
	"data": [
		{"partition_id": "<partition_id>",
		 "checksum": "<SHA256 of encrypted partition> | null"},
	   ...
	]
}
```

Then for each partition_id, a  POST request is sent with the following metadata as well as the
partition data depending on context, such as if `"checksum"` is null.

```json
{
	"partition_id": "<partition_id>",
	"salt": "<salt>",
	"symmetric_key": "<encrypted symmetric key>",
	"next_id": "<encrypted next_id> | null",
	"partition_checksum": "<encrypted partition_checksum>",
	"partition_data": "<encrypted partition_data>",
	"metadata": {
		"path": "<encrypted_path",
		"modification_time": "<encrypted modification_time>"
	}
} 
```

Note that each initial request will start the following:

```json
{
	"userId": "<userId>",
	"token": "<auth token>"
}
```
