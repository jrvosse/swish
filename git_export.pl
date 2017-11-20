:- module(git_export, [
              git_export/1  % +GitDir  export gitty to GitDir
          ]).
:- use_module(lib/storage).
:- use_module(library(git)).
:- use_module(library(filesex)).


git_export(Target) :-
    git_init(Target),
    gitty_heads(Heads),
    find_all_commits(Heads, Commits0),
    keysort(Commits0, Commits),
    commits_to_git(init, init, Commits, Tip, [directory(Target)]),
    catch(git(['update-ref', 'refs/heads/master', Tip],
              [directory(Target)]), _, fail).

gitty_heads(Heads) :-
    findall(Commit,
            (   storage_file(_File, _Data, Meta),
                get_dict(commit, Meta, Commit)
            ),
            Heads).

author_url(X,X).
author_email(_X, 'anonymous@example.com').

find_author(Commit, Author) :-
    get_dict(author, Commit, Author),
    !.
find_author(Commit, Author) :-
    get_dict(previous, Commit, Prev),
    storage_file(Prev, _, Meta),
    find_author(Meta, Author),
    !.
find_author(_, anonymous).

git_init(Target) :-
    exists_directory(Target),
    !.

git_init(Target) :-
    make_directory_path(Target),
    git([init, '--bare'], [directory(Target)]).


find_all_commits([], []).
find_all_commits([H|T], Commits) :-
    find_all_previous_commits(H, HeadCommits),
    find_all_commits(T, TailCommits),
    append(HeadCommits, TailCommits, Commits).

find_all_previous_commits(C, Commits) :-
    storage_file(C, Data, Meta),
    (   get_dict(previous, Meta, Prev)
    ->  find_all_previous_commits(Prev, PrevCommits)
    ;   PrevCommits = []
    ),
    Meta1 = Meta.put(content,Data),
    Commits = [Meta.time-Meta1|PrevCommits].

store_git_object(Meta, Hash, Options) :-
        option(type(Type), Options, blob),
        (   Type == tree
        ->  Encoding = binary
        ;   Encoding = utf8
        ),
        tmp_file_stream(Encoding, Tmp, Stream),
        write(Stream, Meta.content), close(Stream),
        catch(git(['hash-object', '-w', '-t', Type, Tmp]
                 ,[output(Codes)|Options]), _, fail),
        atom_codes(HashN, Codes),
        sub_atom(HashN, 0, _, 1, Hash). % remove trailing new line ...

commits_to_git(_,Head,[], Head, _Options) :- !.
commits_to_git(init, init, [_-H|T], Tip, Options) :-
    !,
    store_blob(H, Options),
    update_tree(H, '', Tree, TreeContent, Options),
    store_commit(H, Tree, init, Hash, Options),
    commits_to_git(TreeContent, Hash, T, Tip, Options).
commits_to_git(TreeContent, GitParent, [_-H|T], Tip, Options) :-
    !,
    store_blob(H, Options),
    update_tree(H, TreeContent, Tree, NewTreeContent, Options),
    store_commit(H, Tree, GitParent, Hash, Options),
    commits_to_git(NewTreeContent, Hash, T, Tip, Options).

update_tree(Meta, OldContent, TreeHash, NewContent, Options) :-
    gv_hash_atom(Codes, Meta.data),
    atom_codes(HashCode,Codes),
    format(atom(Hdr), '100644 ~w\u0000', [Meta.name]),
    atom_concat(Hdr, HashCode, A),
    atom_concat(OldContent, A, NewContent),
    store_git_object(_{content:NewContent}, TreeHash, [type(tree)|Options]).

store_commit(H, TreeHash, GitParent, Hash, Options) :-
    find_author(H, Author),
    author_url(Author, AuthorURL),     CommitterURL = AuthorURL,
    author_email(Author, AuthorEmail), CommitterEmail = AuthorEmail,
    format_time(atom(GitTimeStamp), '%s %z', H.time), % Git time format
    (   get_dict(commit_message, H, Comment) -> true; Comment = ''),
    (   GitParent = init
    ->  ParentLine = ''
    ;   format(atom(ParentLine), 'parent ~w\n', [GitParent])
    ),
    format(atom(CommitContent),
               'tree ~w~n~wauthor ~w <~w> ~w~ncommitter ~w <~w> ~w~n~n~w~n',
               [TreeHash, ParentLine,
                AuthorURL, AuthorEmail, GitTimeStamp,
                CommitterURL, CommitterEmail, GitTimeStamp,
                Comment]),
    store_git_object(_{content:CommitContent}, Hash, [type(commit)|Options]).


store_blob(H, Options) :-
    store_git_object(H, _Hash, Options).

%%      gv_hash_atom(+Codes, -Hash) is det.
%       gv_hash_atom(-Codes, +Hash) is det.
%
%       Bi-directional version of hash_atom/2 ...
%
gv_hash_atom(Codes, Hash) :-
        nonvar(Codes),
        !,
        hash_atom(Codes, Hash).

gv_hash_atom(Codes, Hash) :-
        nonvar(Hash),
        atom_chars(Hash, Chars),
        phrase(hex_bytes(Chars), Codes).

hex_bytes([High,Low|T]) -->
        { char_type(High, xdigit(H)),
          char_type(Low,  xdigit(L)),
          Code is 16*H + L
        },
        [Code],
        hex_bytes(T).
hex_bytes([]) --> [].
