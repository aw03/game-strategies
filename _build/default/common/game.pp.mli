Caml1999N031����            /common/game.mli����  "r  �  x  ������1ocaml.ppx.context��&_none_@@ �A����������)tool_name���*ppx_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������,library-name�@�@@����:game_strategies_common_lib��.<command-line>A@A�A@[@@��A@@�A@\@@@@�@@�������@�@@@�@@�@@@@�@@@�@��������$Core��/common/game.mliA@F�A@J@@��A@@@@�@������)Game_kind��CLS�CL\@�����A�    �!t��Dcj�Dck@@@���+Tic_tac_toe��"Ent�#En@@�@@��&Enr@@��$Omok��,F @ F�-F @ J@@�@@��0F @ D@@@A@���(deriving��6G K P�7G K X@�����������'sexp_of��EG K Y�FG K `@�@@@�����%equal��OG K b�PG K g@�@@@�����&bin_io��YG K i�ZG K o@�@@@@�@@@@@��^G K M�_G K p@@��aDce@@�@�����������-ocaml.warning��&_none_A@ �A�������#-32@@@@@�A���Р)sexp_of_th��@����on@n@@������(Sexplib0$Sexp!t�wvA@@@@@@@11������������/Ppx_compare_lib%Equal!S>>@����!tC    ���@@@A�������@J@@@J@J@J@J������������(Bin_prot'Binable!SYY@����!t^    ���@@@A�������@e@@@e@e@e@e@M@M���)ocaml.doc\�������'@inlined@d@@@d@d���+merlin.hideh�@�@^���Р)to_string���I r x��I r �@��@����!t���I r ���I r �@@�@@@����&string���I r ���I r �@@�@@@�@@@@@���I r t@�@���Р-to_string_hum�� J � ��J � �@��@����!t��
J � ��J � �@@�@@@����&string��J � ��J � �@@�@@@�@@@@@��J � �@�@���Р,board_length��!N!�"N-@��@����!t��+N0�,N1@@�@@@����#int��4N5�5N8@@�@@@�@@@@���)ocaml.doc될�����	` [board_length] returns the length of the board. 3 for [ Tic_tac_toe ]
      and 15 for [Omok]. ��EL � ��FM �@@@@@@@��HN@�@���Р*win_length��QR���RR��@��@����!t��[R���\R��@@�@@@����#int��dR���eR��@@�@@@�@@@@���0�������	e [win_length] returns the winning length of the board. 3 for
      [ Tic_tac_toe ] and 5 for [Omok]. ��tP:<�uQ|�@@@@@@@��wR��@�@@��zCL_�{S��@@@��}CLL@�@������%Piece���U����U��@�����A�    �!t���V����V��@@@���!X���W����W��@@�@@���W��@@��!O���X����X��@@�@@���X��@@@A@���(deriving���Y����Y�@�����������'sexp_of���Y���Y�@�@@@�����%equal���Y���Y�@�@@@�����&bin_io���Y���Y�@�@@@�����)enumerate���Y���Y�%@�@@@@�"@@@@@���Y����Y�&@@���V��@@�@��������������������#-32�@�@@@�@��A���Р)sexp_of_tn��@����ut@t@@������~}�zyA@@@@@@@**������������/Ppx_compare_lib%Equal!S77@����!t<    ���@@@A�������@C@@@C@C@C@C������������(Bin_prot'Binable!SRR@����!tW    ���@@@A�������@^@@@^@^@^@^������������1Ppx_enumerate_lib*Enumerable!Smm@����!tr    ���@@@A�������@y@@@y@y@y@y@e@e����򐠠������@�@@@�@�������@@s���Р)of_string��s[(.�t[(7@��@����&string��}[(:�~[(@@@�@@@����!t���[(D��[(E@@�@@@�@@@@@���[(*@�@���Р)to_string���\FL��\FU@��@����!t���\FX��\FY@@�@@@����&string���\F]��\Fc@@�@@@�@@@@@���\FH@�@���Р$flip���_����_��@��@����!t���_����_��@@�@@@����!t���_����_��@@�@@@�@@@@@���_��@�@@���U����`��@@@���U��@�@������(Position���b����b��@�����A�    �!t���hQX��hQY@@@��Р#row���i\b��i\e@@����#int���i\h��i\k@@�@@@���jlq@@�Р&column��jlr�jlx@@����#int��jl{�jl~@@�@@@�@@@A@���(deriving��l���l��@�����������'sexp_of��#l���$l��@�@@@�����%equal��-l���.l��@�@@@�����&bin_io��7l���8l��@�@@@�����'compare��Al���Bl��@�@@@@�"@@@@@��Fl���Gl��@@��IhQS@@�@������������琠�����#-32�@�@@@�@�A���Р)sexp_of_t}��@������@�@@�����������A@@@@@@@**������������/Ppx_compare_lib%Equal!S77@����!t<    ���@@@A�������@C@@@C@C@C@C������������(Bin_prot'Binable!SRR@����!tW    ���@@@A�������@^@@@^@^@^@^������������/Ppx_compare_lib*Comparable!Smm@����!tr    ���@@@A�������@y@@@y@y@y@y@e@e����W��������^@^@@@^@^����a�@�@s���Р)to_string���n����n��@��@����!t���n����n��@@�@@@����&string���n����n��@@�@@@�@@@@@���n��@�@���Р)in_bounds���o����o��@��@����!t��o���o��@@�@@@���)game_kind�����)Game_kind!t��o���o��@@�@@@����$bool��o��o�@@�@@@��o��@@@�@@@@@��"o��@�@���Р$down��+r8>�,r8B@��@����!t��5r8E�6r8F@@�@@@����!t��>r8J�?r8K@@�@@@�@@@@���
��������	' [down t] is [t]'s downwards neighbor. ��Nq	�Oq	7@@@@@@@��Qr8:@�@���Р%right��Zu~��[u~�@��@����!t��du~��eu~�@@�@@@����!t��mu~��nu~�@@�@@@�@@@@���9#�������	) [right t] is [t]'s rightwards neighbor. ��}tMO�~tM}@@@@@@@���u~�@�@���Р"up���x����x��@��@����!t���x����x��@@�@@@����!t���x����x��@@�@@@�@@@@���hR�������	# [up t] is [t]'s upwards neighbor. ���w����w��@@@@@@@���x��@�@���Р$left���{��{@��@����!t���{��{@@�@@@����!t���{��{@@�@@@�@@@@������������	' [left t] is [t]'s leftwards neighbor. ���z����z� @@@@@@@���{@�@���Р+all_offsets��� @���� @��@����$list��� @���� @��@���@����!t��� @���� @��@@�@@@����!t�� @��� @��@@�@@@�@@@@�� @��@@@@������������	� [all_offsets] is a list of functions to compute all 8 neighbors of a
      cell (i.e. left, up-left, up, up-right, right, right-down, down,
      down-left). ��}���@@@@@@@�� @��)@�*@�����������*Comparable'S_plain��( B���) B��@�@@����!t��1 B��2 B�@    �@@@A�����!t��< B��= B�@@�@@@@��@ B�@@�@@��C B��@@�@@��Fb���G C@@@��Ib��@�@������*Evaluation��S E�T E$@�����A�    �!t��_ F+2�` F+3@@@���,Illegal_move��g G6<�h G6H@@�@@��k G6:@@��.Game_continues��q HIO�r HI]@@�@@��u HIM@@��)Game_over��{ I^d�| I^m@@��Р&winner��� I^s�� I^y@@����&option��� I^��� I^�@������%Piece!t��� I^|�� I^�@@�@@@@�@@@�@@@@��� I^b�� I^�@@@A@���(deriving��� J���� J��@�����������'sexp_of��� J���� J��@�@@@�����&bin_io��� J���� J��@�@@@�����%equal��� J���� J��@�@@@@�@@@@@��� J���� J��@@��� F+-@@�@�����������ml�������#-32t@t@@@t@t�A���Р)sexp_of_t���@������@�@@������ihg���A@@@@@@@**������������(Bin_prot'Binable!S77@����!t<    ���@@@A�������@C@@@C@C@C@C������������/Ppx_compare_lib%Equal!SRR@����!tW    ���@@@A�������@^@@@^@^@^@^@J@J���f��������e�@�@@@�@Ƞ��dː@�@X@��< E'�= K��@@@��? E@�@���A�    �!t��I M���J M��@@@��Р)game_kind��Q N���R N��@@�����)Game_kind!t��[ N���\ N��@@�@@@��_ O��@@�Р%board��e O���f O��@@������(Position#Map!t��q O���r O��@������%Piece!t��| O���} O��@@�@@@@�@@@�@@@A@���(deriving��� Q�� Q@�����������'sexp_of��� Q�� Q@�@@@�����&bin_io��� Q�� Q@�@@@@�@@@@@��� Q�� Q @@��� M��@@�@�����������FE�������#-32M@M@@@M@M�A���Р)sexp_of_t{��@������@�@@������BA@���A@@@@@@@**������������(Bin_prot'Binable!S77@����!t<    ���@@@A�������@C@@@C@C@C@C@/@/���$�������#�@�@@@�@����"��@�@=���Р%empty��  S"&� S"+@��@�����)Game_kind!t�� S".� S"9@@�@@@����!t�� S"=� S">@@�@@@�@@@@@�� S""@�@@