CREATE OR REPLACE
FUNCTION parseText(doc bytea)
RETURNS text language plpgsql immutable AS $$
BEGIN
	return convert_from(doc, 'utf8');
EXCEPTION WHEN others THEN
	return encode(doc, 'escape');
END; $$;
