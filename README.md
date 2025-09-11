📘 HaskellWeb – Minimalistický HTTP server

Jednoduchý HTTP server v Haskellu.
Používá knihovnu network pro práci se sokety a attoparsec pro parsování požadavků.
Obsahuje malý systém middlewarů a routování ve stylu ASP.NET

🚀 Funkce
	•	Parsování HTTP požadavků (GET, POST, ...)
	•	Serializace platných HTTP/1.1 requestů 
	•	Routování podle zadaných parametrů 
	•	Middleware podpora (logging, CORS, podpora pro HTTP status 500)
	•	Pomocníci pro běžné HTTP odpovědi (200, 400, 404, 500)

📂 Struktura projektu

app/
 ├── BuiltinMiddleware.hs   -- Logging, CORS, ošetření chyb
 ├── HTTP/Parser.hs         -- Parser HTTP požadavků
 ├── HTTP/Render.hs         -- Serializace odpovědi
 ├── Main.hs                -- Vstupní bod
 ├── Middleware.hs          -- Definice typů pro middleware
 ├── Routing.hs             -- Pomocné funkce pro routování
 ├── Server.hs              -- TCP server smyčka & obsluha spojení
 └── Types.hs               -- Datové typy Request/Response + helpers

🏗️ Základní typy

type Header = (ByteString, ByteString)
type Handler = Request -> IO Response
type Middleware = Handler -> Handler

	•	Request: { method, path, query, headers, body }
	•	Response: { statusCode, responseHeaders, responseBody }

🔧 Middleware
	•	logMw – loguje požadavky a odpovědi
	•	corsMw – přidá hlavičku Access-Control-Allow-Origin: *
	•	recoverMw – zachytí výjimky → vrátí 500 Internal Server Error

📌 Routování
	•	routeExact "/ping" → přesná shoda cesty
	•	notFound → fallback handler (404)

Příklad použití

Main.hs

main :: IO ()
main = run "127.0.0.1:8080" $
       notFound
       |-> logMw
       |-> corsMw
       |-> routeExact "/ping" (const $ pure $ ok200 "pong")

Spuštění serveru:

cabal run haskellweb

Test:

curl http://127.0.0.1:8080/ping
# -> pong

✅ Omezení
	•	Pouze HTTP/1.0 a HTTP/1.1
	•	Nepodporuje chunked transfer encoding
	•	Jednovláknový (každé spojení se obsluhuje sekvenčně)
	•	Základní ošetření chyb

📊 Diagram workflow

+---------+     +-----------------+     +--------------+     +---------------+     +----------+
| Klient  | --> | Server přijme   | --> | Middleware   | --> | Routování     | --> | Handler  |
|         |     | požadavek       |     | (log, CORS)  |     | (cesta, met.) |     | / 404    |
+---------+     +-----------------+     +--------------+     +---------------+     +----------+
                                                                                          |
                                                                                          v
                                                                                  +-----------------+
                                                                                  | Odpověď klientu |
                                                                                  +-----------------+
