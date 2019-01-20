# Peer-lending Directory

> Restful directory for Peer Lending project

If gradle wrapper is missing, run `downloadWrapper` task from the IDE

## Build Instructions
```shell
# Start directory server on port http://localhost:8080
$ ./gradlew run
```

## Example Responses

GET /exchanges

```json
{
    "code": 200,
    "data": [
        {
            "host": "localhost",
            "name": "exch3",
            "port": 5433
        },
        {
            "host": "localhost",
            "name": "exch2",
            "port": 5432
        },
        {
            "host": "localhost",
            "name": "exch1",
            "port": 5431
        }
    ]
}
```

GET /companies/Netflix

```json
{
    "code": 200,
    "data": {
        "auctions": [],
        "exchange": {
            "host": "localhost",
            "name": "exch1",
            "port": 5431
        },
        "name": "Netflix"
    }
}
```

GET /companies/Something

```json
{
    "code": 404,
    "message": "Company Something does not exist"
}
```
