%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Unit tests of the unicode_utils module.
%%% @end
%%%--------------------------------------------------------------------
-module(unicode_utils_tests).
-author("Lukasz Opiola").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("test/test_utils.hrl").


-define(test(Input, Expectation), begin
    ?assertEqual(
        Expectation,
        unicode_utils:remove_diacritics(Input)
    ),
    ?assertEqual(
        str_utils:unicode_list_to_binary(Expectation),
        unicode_utils:remove_diacritics(str_utils:unicode_list_to_binary(Input))
    )
end).


remove_diacritics_test() ->
    ?test("hello, world", "hello, world"),
    ?test("你好，世界", "你好,世界"),
    ?test("François", "Francois"),
    ?test("Zażółć Gęślą Jaźń", "Zazolc Gesla Jazn"),
    ?test(
        str_utils:binary_to_unicode_list(<<1,2,3,4,5,6,7,8,9>>),
        str_utils:binary_to_unicode_list(<<1,2,3,4,5,6,7,8,9>>)
    ),
    ?test(
        "Dès Noël, où un zéphyr haï me vêt de glaçons würmiens, je dîne d’exquis rôtis de bœuf au kir, à l’aÿ d’âge mûr, &cætera.",
        "Des Noel, ou un zephyr hai me vet de glacons wurmiens, je dine d’exquis rotis de boeuf au kir, a l’ay d’age mur, &caetera."
    ),
    ?test(
        "Falsches Üben von Xylophonmusik quält jeden größeren Zwerg.",
        "Falsches Uben von Xylophonmusik qualt jeden grosseren Zwerg."
    ),
    ?test(
        "Љубазни фењерџија чађавог лица хоће да ми покаже штос.",
        "Љубазни фењерџија чађавог лица хоће да ми покаже штос."
    ),
    ?test(
        "Ljubazni fenjerdžija čađavog lica hoće da mi pokaže štos.",
        "Ljubazni fenjerdzija cadavog lica hoce da mi pokaze stos."
    ),
    ?test(
        "Quizdeltagerne spiste jordbær med fløde, mens cirkusklovnen Walther spillede på xylofon.",
        "Quizdeltagerne spiste jordbaer med flode, mens cirkusklovnen Walther spillede pa xylofon."
    ),
    ?test(
        "Kæmi ný öxi hér ykist þjófum nú bæði víl og ádrepa.",
        "Kaemi ny oxi her ykist bjofum nu baedi vil og adrepa."
    ),
    ?test(
        "Glāžšķūņa rūķīši dzērumā čiepj Baha koncertflīģeļu vākus.",
        "Glazskuna rukisi dzeruma ciepj Baha koncertfligelu vakus."
    ),
    ?test(
        "Ë À Ì Â Í Ã Î Ä Ï Ç Ò È Ó É Ô Ê Õ Ö ê Ù ë Ú î Û ï Ü ô Ý õ â û ã ÿ ç",
        "E A I A I A I A I C O E O E O E O O e U e U i U i U o Y o a u a y c"
    ),
    ?test(
        "Š š Ð Ž ž À Á Â Ã Ä Å Ç È É Ê Ë Ì Í Î Ï Ñ Ń Ò Ó Ô Õ Ö Ø Ù Ú Û Ü Ý Þ þ à á â ã ä å ç",
        "S s D Z z A A A A A A C E E E E I I I I N N O O O O O O U U U U Y B b a a a a a a c"
    ),
    ?test(
        "è é ê ë ì í î ï ð ñ ń ò ó ô õ ö ø ù ú û ü ý þ ÿ ƒ ă î â ș ț Ă Î Â Ș Ț",
        "e e e e i i i i d n n o o o o o o u u u u y b y f a i a s t A I A S T"
    ),
    ?test(
        "æ Æ ǽ Ǽ œ Œ ß ẞ",
        "ae AE ae AE oe OE ss SS"
    ),
    ?test(
        "ç á é í ó ú à è ì ò ù ä ë ï ö ü ÿ â ê î ô û å ø Ø Å Á À Â Ä È É Ê Ë Í Î Ï Ì Ò Ó Ô Ö Ú Ù Û Ü Ÿ Ç",
        "c a e i o u a e i o u a e i o u y a e i o u a o O A A A A A E E E E I I I I O O O O U U U U Y C"
    ),
    ?test(
        "ÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞàáâãäåçèéêëEËËËìíîïðñòóôõöøùúûýýÿŔŕ",
        "AAAAAACEEEEIIIIDNOOOOOOUUUUYBaaaaaaceeeeEEEEiiiidnoooooouuuyyyRr"
).



-endif.
