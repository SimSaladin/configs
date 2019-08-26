-- demlo script
help([=[
Search and replace among all tags.

GLOBAL VARIABLES

- sub: An array of {[[regular expression]], [[replacement string]]}.
  sub is not an associative array since order must be guaranteed.

EXAMPLES

	sub = {
	       {[[(\PL+)']], '$1"'},
	       {[['(\PL+)]], '"$1'},
	       {"'$", '"'}
	      }

The previous substitution rules replace simple quotes by double quotes.
This can be undesirable in some contexts, such as "Rock 'n' Roll".

]=])

-- sub = sub

-- Default: Replace various type of single quotes by "'".
-- Replace curly braces by square braces.
local subst = {
	{'[´`’]', "'"},
	{'{', '['},
	{'}', ']'},
    {[[(^|\W)Vol\.(\d+($|\W))]], '${1Vol. $2'},

}
for _, r in ipairs(sub or {}) do
        table.insert(subst, r)
end

-- WARNING: We cannot use the second argument returned by 'pairs' as it will
-- change inside the loop.
for k, _ in pairs(output.tags) do
	for _, rule in ipairs(subst) do
		output.tags[k] = output.tags[k]:gsub(rule[1], rule[2])
	end
end
