% $begin all_ok$$ $newlinech %$$
%
% $section Run All of the non_par Tests$$
%
% $table
% $bold Syntax$$ $cnext $syntax%all_ok%$$ 
% $tend 
%
% $fend 20$$
%
% $head Purpose$$
% The command
% $codep
%	all_ok
% $$
% runs all of the non_par tests and prints the results.
% The actual source code for this routine is included below
%
% $head Source Code$$
% $nospell $codep
function [ok] = all_ok()
ok   = true;
ok   = ok & one_ok('npipp_ok');
return
% ------------------------------------
function [ok] = one_ok(test_name)
ok = feval(test_name);
if( ok )
	['ok:    ', test_name]
else
	['error: ', test_name]
end
return
% $$ $$
%
% $end
