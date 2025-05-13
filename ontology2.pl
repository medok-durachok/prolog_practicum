% изменяемые предикаты
:- dynamic dish/6.
:- dynamic ingredient/3.

keyword_class("ржаной хлеб", black_bread).
keyword_class("пшеничный хлеб", white_bread).
keyword_class("мультизлаковый хлеб", cereal_bread).

keyword_class("хлеб", bread).
keyword_class("пицца", pizza).
keyword_class("паста", pasta).


class_default(bread, type, "выпечка").
class_default(bread, cuisine, "общее").
class_default(bread, cooking_method, "выпекание").
class_default(bread, ingredient, "соль").
class_default(bread, ingredient, "вода").

% хлеб
class_default(black_bread, parent, bread).
class_default(black_bread, ingredient, "ржаная мука").

class_default(white_bread, parent, bread).
class_default(white_bread, ingredient, "пшеничная мука").

class_default(cereal_bread, parent, bread).
class_default(cereal_bread, ingredient, "пшеничная мука").
class_default(cereal_bread, ingredient, "ржаная мука").
class_default(cereal_bread, ingredient, "полбяная мука").

% пицца
class_default(pizza, cuisine, "итальянская кухня").
class_default(pizza, cooking_method, "выпекание").
class_default(pizza, ingredient, "тесто").
class_default(pizza, ingredient, "сыр").

% паста
class_default(pasta, cuisine, "итальянская кухня").
class_default(pasta, cooking_method, "варка").
class_default(pasta, ingredient, "паста").

% приготовления по умолчанию
type_default("суп", cooking_method, "варка").
type_default("соус", cooking_method, "смешивание").
type_default("выпечка", cooking_method, "выпекание").
type_default("салат", cooking_method, "смешивание").
type_default("основное блюдо", cooking_method, "не задано").
type_default("десерт", cooking_method, "не задано").
type_default("закуска", cooking_method, "не задано").

type_default("суп", cuisine, "общее").
type_default("соус", cuisine, "общее").
type_default("выпечка", cuisine, "общее").
type_default("салат", cuisine, "общее").
type_default("десерт", cuisine, "общее").
type_default("основное блюдо", cuisine, "общее").
type_default("закуска", cuisine, "общее").


% наследование всех атрибутов блюда
from_parent_dish_properties(Name, DefCuisine, DefType, DefIngredients, DefCookingMethod, 
                        NewCuisine, NewType, NewIngredients, NewCookingMethod) :-
    (   nonvar(DefCuisine) -> NewCuisine = DefCuisine;   
        find_dish_attribute(Name, DefType, cuisine, NewCuisine) ->  true;   
        NewCuisine = ""
    ),
    (   nonvar(DefType) ->  NewType = DefType;
        find_dish_attribute(Name, DefType, type, NewType) ->  true;
        NewType = ""
    ),
    (   nonvar(DefCookingMethod) -> NewCookingMethod = DefCookingMethod;
        find_dish_attribute(Name, DefType, cooking_method, NewCookingMethod) ->  true;
        NewCookingMethod = "" 
    ),
    (   nonvar(DefIngredients) -> AllIngredients = DefIngredients;   
        AllIngredients = []
    ),
    find_dish_ingredients(Name, DefType, AllIngredients, NewIngredients).


% определение атрибута блюда
find_dish_attribute(Name, Type, Attribute, Value) :-
    (   keyword_class(Keyword, Class),
        sub_string(Name, _, _, _, Keyword) ->
        (from_parent_attribute(Class, Attribute, Value) -> true; Value = "");
        (from_parent_from_type(Type, Attribute, Value) -> true; Value = "")
    ).


% рекурсивное наследование
from_parent_attribute(Class, Attribute, Value) :-
    from_parent_attribute_recursive(Class, Attribute, Value, []).

from_parent_attribute_recursive(Class, Attribute, Value, Visited) :-
    \+ member(Class, Visited),  % проверка на зацикливание
    (   class_default(Class, Attribute, Value);
        class_default(Class, parent, ParentClass), from_parent_attribute_recursive(ParentClass, Attribute, Value, [Class|Visited])
    ).


% наследование атрибута через тип
from_parent_from_type(Type, Attribute, Value) :-
    type_default(Type, Attribute, Value).


% наследование ингредиентов
find_dish_ingredients(Name, Type, Ingredients, NewIngredients) :-
    (   keyword_class(Keyword, Class),
        sub_string(Name, _, _, _, Keyword) -> findall(Ingredient, from_parent_attribute(Class, ingredient, Ingredient), ClassIngredients);
                                              findall(Ingredient, from_parent_from_type(Type, ingredient, Ingredient), ClassIngredients)
    ),
    append(Ingredients, ClassIngredients, AllIngredients),
    list_to_set(AllIngredients, NewIngredients).


% вывод всех блюд
print_all_dishes :- forall(dish(ID, Name, Cuisine, Type, Ingredients, CookingMethod), print_dish(ID, Name, Cuisine, Type, Ingredients, CookingMethod)).


print_dish(ID, Name, Cuisine, Type, Ingredients, CookingMethod) :-
    (   var(Cuisine) -> find_dish_attribute(Name, Type, cuisine, NewCuisine) ; NewCuisine = Cuisine ),
    (   var(Type) -> find_dish_attribute(Name, Type, type, NewType) ; NewType = Type ),
    (   var(CookingMethod) -> find_dish_attribute(Name, Type, cooking_method, NewCookingMethod) ; NewCookingMethod = CookingMethod ),
    find_dish_ingredients(Name, Type, Ingredients, NewIngredients),
    maplist(find_ingredient_name, NewIngredients, IngredientNames),
    write("ID: "), write(ID), nl,
    write("Название: "), write(Name), nl,
    write("Кухня: "), write(NewCuisine), nl,
    write("Тип: "), write(NewType), nl,
    write("Ингредиенты: "), write(IngredientNames), nl,
    write("Способ приготовления: "), write(NewCookingMethod), nl, nl.



% Поиск названия ингредиента по ID
find_ingredient_name(ID, Name) :- ingredient(ID, Name, _), !.
find_ingredient_name(ID, Name) :- dish(ID, Name, _, _, _, _), !.
find_ingredient_name(Name, Name).
find_ingredient_name(_, "Неизвестный ингредиент").


% сохранение базы данных
save_database(Filename) :-
    open(Filename, write, Stream, [encoding(utf8)]),
    forall(dish(ID, Name, Cuisine, Type, Ingredients, CookingMethod),
           (   write(Stream, dish(ID, Name, Cuisine, Type, Ingredients, CookingMethod)), write(Stream, '.'), nl(Stream))),
    forall(ingredient(ID, Name, Type),
           (   write(Stream, ingredient(ID, Name, Type)), write(Stream, '.'), nl(Stream))),
    close(Stream).


% поиск по названию блюда
search_dish_by_name(Name) :-
    findall((ID, DishName, Cuisine, Type, Ingredients, CookingMethod),
        (dish(ID, DishName, Cuisine, Type, Ingredients, CookingMethod), sub_string(DishName, _, _, _, Name)), Dishes
    ),
    (   Dishes \= [] 
    ->  forall(member((ID, DishName, Cuisine, Type, Ingredients, CookingMethod), Dishes),
            print_dish(ID, DishName, Cuisine, Type, Ingredients, CookingMethod)
        );
        write("Ничего не найдено."), nl
    ).
search_dish_by_name(_).


% поиск блюда по кухне
search_dish_by_cuisine(CuisineFragment) :-
    findall(
        (ID, Name, ResCuisine, ResType, ResIngredients, ResCookingMethod),
        (
            dish(ID, Name, Cuisine, Type, Ingredients, CookingMethod),
            from_parent_dish_properties(Name, Cuisine, Type, Ingredients, CookingMethod, ResCuisine, ResType, ResIngredients, ResCookingMethod),
            sub_string(ResCuisine, _, _, _, CuisineFragment)
        ),
        Dishes
    ),
    (   Dishes \= []
    ->  forall(member((ID, Name, ResCuisine, ResType, ResIngredients, ResCookingMethod), Dishes),
            print_dish(ID, Name, ResCuisine, ResType, ResIngredients, ResCookingMethod)
        );
        write("Ничего не найдено."), nl
    ).

% поиск блюда по типу
search_dish_by_type(TypeFragment) :-
    findall((ID, Name, ResCuisine, ResType, ResIngredients, ResCookingMethod),
        (
            dish(ID, Name, Cuisine, Type, Ingredients, CookingMethod),
            from_parent_dish_properties(Name, Cuisine, Type, Ingredients, CookingMethod, ResCuisine, ResType, ResIngredients, ResCookingMethod),
            sub_string(ResType, _, _, _, TypeFragment)
        ),
        Dishes
    ),
    (   Dishes \= []
    ->  forall(member((ID, Name, ResCuisine, ResType, ResIngredients, ResCookingMethod), Dishes),
            print_dish(ID, Name, ResCuisine, ResType, ResIngredients, ResCookingMethod)
        );
        write("Ничего не найдено."), nl
    ).


% поиск блюда по способу приготовления
search_dish_by_cookingmethod(MethodFragment) :-
    findall((ID, Name, ResCuisine, ResType, ResIngredients, ResCookingMethod),
        (
            dish(ID, Name, Cuisine, Type, Ingredients, CookingMethod),
            from_parent_dish_properties(Name, Cuisine, Type, Ingredients, CookingMethod, ResCuisine, ResType, ResIngredients, ResCookingMethod),
            sub_string(ResCookingMethod, _, _, _, MethodFragment)
        ),
        Dishes
    ),
    (   Dishes \= []
    ->  forall(member((ID, Name, ResCuisine, ResType, ResIngredients, ResCookingMethod), Dishes),
            print_dish(ID, Name, ResCuisine, ResType, ResIngredients, ResCookingMethod)
        );
        write("Ничего не найдено."), nl
    ).


% поиск блюда по ингредиенту
search_dish_by_ingredient(IngredientFragment) :-
    findall((ID, Name, ResCuisine, ResType, ResIngredients, ResCookingMethod),
        (
            dish(ID, Name, Cuisine, Type, Ingredients, CookingMethod),
            from_parent_dish_properties(Name, Cuisine, Type, Ingredients, CookingMethod, ResCuisine, ResType, ResIngredients, ResCookingMethod),
            maplist(find_ingredient_name, ResIngredients, IngredientNames),
            member(IngredientFragment, IngredientNames)
        ),
        Dishes
    ),
    (   Dishes \= []
    ->  forall(member((ID, Name, ResCuisine, ResType, ResIngredients, ResCookingMethod), Dishes),
            print_dish(ID, Name, ResCuisine, ResType, ResIngredients, ResCookingMethod)
        );
        write("Ничего не найдено."), nl
    ).


% поиск ингредиента по названию 
search_ingredient_by_name(Name) :-
    (   ingredient(ID, Name, Type)
    ->  write("ID: "), write(ID), nl,
        write("Ингредиент: "), write(Name), nl,
        write("Тип: "), write(Type), nl, nl,
        fail;
        write("Ингредиент не найден."), nl
    ).
search_ingredient_by_name(_).


% поиск ингредиента по типу
search_ingredient_by_type(Type) :-
    (   ingredient(ID, Name, Type)
    ->  write("ID: "), write(ID), nl,
        write("Ингредиент: "), write(Name), nl,
        write("Тип: "), write(Type), nl, nl,
        fail;
        write("Ингредиент не найден."), nl
    ).
search_ingredient_by_type(_).


% cравнение двух блюд по их свойствам
compare_dishes(Name1, Name2) :-
    (   \+ dish(_, Name1, _, _, _, _)
    ->  write("Блюдо "), write(Name1), write(" не найдено."), nl;
        \+ dish(_, Name2, _, _, _, _)
    ->  write("Блюдо "), write(Name2), write(" не найдено."), nl;
        dish(_, Name1, Cuisine1, Type1, Ingredients1, CookingMethod1),
        dish(_, Name2, Cuisine2, Type2, Ingredients2, CookingMethod2),

        from_parent_dish_properties(Name1, Cuisine1, Type1, Ingredients1, CookingMethod1, NewCuisine1, NewType1, NewIngredients1, NewCookingMethod1),
        from_parent_dish_properties(Name2, Cuisine2, Type2, Ingredients2, CookingMethod2, NewCuisine2, NewType2, NewIngredients2, NewCookingMethod2),

        sort(NewIngredients1, SortedIngredients1),
        sort(NewIngredients2, SortedIngredients2),
        maplist(find_ingredient_name, SortedIngredients1, IngredientNames1),
        maplist(find_ingredient_name, SortedIngredients2, IngredientNames2),

        % Сравнение кухни
        (   NewCuisine1 \= NewCuisine2
        ->  write('Кухни блюд различны: '), write(NewCuisine1), write(' и '), write(NewCuisine2), nl;
            write('Кухни блюд совпадают: '), write(NewCuisine1), nl
        ),
        (   NewType1 \= NewType2
        ->  write('Типы блюд различны: '), write(NewType1), write(' и '), write(NewType2), nl;
            write('Типы блюд совпадают: '), write(NewType1), nl
        ),
        (   NewCookingMethod1 \= NewCookingMethod2
        ->  write('Способы приготовления различны: '), write(NewCookingMethod1), write(' и '), write(NewCookingMethod2), nl;
            write('Способы приготовления совпадают: '), write(NewCookingMethod1), nl
        ),
        (   SortedIngredients1 \= SortedIngredients2
        ->  write('Ингредиенты различны: '), write(IngredientNames1), write(' и '), write(IngredientNames2), nl;
            write('Ингредиенты совпадают: '), write(IngredientNames1), nl
        )
    ).


% сравнение двух ингредиентов
compare_ingredients(Name1, Name2) :-
    (   \+ ingredient(_, Name1, _) 
    ->  write("Ингредиент "), write(Name1), write(" не найден."), nl;
        \+ ingredient(_, Name2, _) 
    ->  write("Ингредиент "), write(Name2), write(" не найден."), nl;
        ingredient(_, Name1, Type1), 
        ingredient(_, Name2, Type2),
        (   Type1 \= Type2
        ->  write('Тип: '), write(Type1), write(' <> '), write(Type2), nl;
            write("Тип ингредиентов совпадает: "), write(Type1), nl
        )
    ).


% получение максимального ID
get_next_dish_id(NextID) :-
    findall(ID, dish(ID, _, _, _, _, _), IDs), % cобираем все существующие ID блюд
    ( IDs == [] -> % если список пуст
        NextID = 1;
    max_list(IDs, MaxID), NextID is MaxID + 1 % увеличиваем максимум на 1
    ).


% проверка наличия ингредиента, предложение добавить
get_ingredient_ids([], []).
get_ingredient_ids([Name | RestNames], [ID | RestIDs]) :-
    ingredient(ID, Name, _),
    get_ingredient_ids(RestNames, RestIDs).
get_ingredient_ids([Name | RestNames], IDs) :-
    \+ ingredient(_, Name, _), % ингредиента нет
    write('Ингредиент "'), write(Name), write('" не найден. Хотите добавить? (да/нет): '),
    read(Answer),
    ( Answer == да ->
        add_ingredient(Name), ingredient(ID, Name, _), get_ingredient_ids(RestNames, RestIDs), IDs = [ID | RestIDs];
      Answer == нет ->
        write('Ингредиент пропущен: '), write(Name), nl, get_ingredient_ids(RestNames, IDs)
    ).


% добавление блюда
add_dish(Name, Cuisine, Type, IngredientNames, CookingMethod) :-
    ( dish(_, Name, _, _, _, _) ->
        write('Блюдо с таким названием уже существует.'), nl;
      get_ingredient_ids(IngredientNames, IngredientIDs),
      ( IngredientIDs == [] ->
          write('Не удалось добавить блюдо, так как ингредиенты отсутствуют.'), nl;
        get_next_dish_id(ID),
        assertz(dish(ID, Name, Cuisine, Type, IngredientIDs, CookingMethod)), % сохраняем
        write('Блюдо добавлено: '), write(ID), nl
      )
    ).



% получение максимального ID ингредиента
get_next_ingredient_id(NextID) :-
    findall(ID, ingredient(ID, _, _), IDs),
    ( IDs == [] -> 
        NextID = 1;
      max_list(IDs, MaxID), 
      NextID is MaxID + 1
    ).


% добавление ингредиента
add_ingredient(Name) :-
    \+ ingredient(_, Name, _), % ингредиент не дублировался
    get_next_ingredient_id(ID),
    write("Введите тип ингредиента: "), read(Type),
    assertz(ingredient(ID, Name, Type)),
    write('Ингредиент добавлен: '), write(ID), nl, !.
add_ingredient(Name) :-
    ingredient(ID, Name, _),
    write('Ингредиент уже существует с ID: '), write(ID), nl.


% удаление блюда по названию
remove_dish(Name) :-
    retract(dish(_, Name, _, _, _, _)),
    write('Блюдо удалено: '), write(Name), nl.


% удаление ингредиента по названию 
remove_ingredient(Name) :-
    findall(NameDish, (
        dish(_, NameDish, _, _, Ingredients, _),
        ingredient(IngredientID, Name, _),
        member(IngredientID, Ingredients)
    ), DishesToRemove),
    forall(member(Dish, DishesToRemove), remove_dish(Dish)),
    retractall(ingredient(_, Name, _)),
    write('Ингредиент удалён из базы данных, и все блюда с ним удалены.'), nl.


% запуск программы
run :- load_files(['C:/uni2024-25/prak/database.pl'],[encoding(utf8)]),
    repeat, 
    write("Что вы хотите сделать?"), nl,
    write("1 — Вывести все блюда"), nl,
    write("2 — Поиск блюд"), nl,
    write("3 — Поиск ингредиентов"), nl, 
    write("4 — Добавить новое блюдо"), nl,
    write("5 — Добавить новый ингредиент"), nl,
    write("6 — Удалить ингредиент"), nl,
    write("7 — Удалить блюдо"), nl,
    write("8 — Сравнить два блюда"), nl,
    write("9 — Сравнить два ингредиента"), nl,
    write("0 — Завершить работу"), nl,
    write("Ваш выбор: "), nl,
    read(Choice), 
    run_choice(Choice), 
    (Choice == 0 -> ! ; fail). 

run_choice(1) :- print_all_dishes, nl, !.
run_choice(2) :- search_dish_menu, nl, !. 
run_choice(3) :- search_ingredient_menu, nl, !. 
run_choice(4) :- 
    write("Введите название блюда: "), read(Name),
    write("Введите кухню: "), read(Cuisine),
    write("Введите тип блюда: "), read(Type),
    write("Введите список ингредиентов: "), read(Ingredients),
    write("Введите способ приготовления: "), read(CookingMethod),
    add_dish(Name, Cuisine, Type, Ingredients, CookingMethod), nl, !.
run_choice(5) :- 
    write("Введите название ингредиента: "), read(Name),
    add_ingredient(Name), nl, !.
run_choice(6) :- 
    write("Введите название ингредиента: "), read(Name),
    remove_ingredient(Name), nl, !.
run_choice(7) :- 
    write("Введите название блюда: "), read(Name),
    remove_dish(Name), nl, !.
run_choice(8) :- 
    write("Введите название первого блюда: "), read(Name1),
    write("Введите название второго блюда: "), read(Name2),
    compare_dishes(Name1, Name2), nl, !.
run_choice(9) :- 
    write("Введите название первого ингредиента: "), read(Name1),
    write("Введите название второго ингредиента: "), read(Name2),
    compare_ingredients(Name1, Name2), nl, !.
run_choice(0) :- save_database('C:/uni2024-25/prak/output_database.pl'), write("До свидания!"), nl, !.
run_choice(_) :- write("Неверный выбор, попробуйте снова."), nl, !.


% поиск блюд
search_dish_menu :-
    write("Поиск блюд по..."), nl,
    write("1 — Названию блюда"), nl,
    write("2 — Кухне"), nl,
    write("3 — Типу"), nl,
    write("4 — Ингредиентам"), nl,
    write("5 — Способу приготовления"), nl,
    write("0 — Назад"), nl,
    write("Ваш выбор: "), nl,
    read(SearchChoice),
    search_choice(SearchChoice).

search_choice(1) :- 
    write("Введите название блюда: "), read(Name),
    search_dish_by_name(Name), nl, !.
search_choice(2) :- 
    write("Введите кухню: "), read(Cuisine),
    search_dish_by_cuisine(Cuisine), nl, !.
search_choice(3) :- 
    write("Введите тип: "), read(Ingredient),
    search_dish_by_type(Ingredient), nl, !.
search_choice(4) :- 
    write("Введите ингредиент: "), read(Cuisine),
    search_dish_by_ingredient(Cuisine), nl, !.
search_choice(5) :- 
    write("Введите способ приготовления: "), read(Ingredient),
    search_dish_by_cookingmethod(Ingredient), nl, !.
search_choice(0) :- run, !.
search_choice(_) :- write("Неверный выбор, попробуйте снова."), nl, search_dish_menu.


% поиск ингредиента
search_ingredient_menu :-
    write("Поиск ингредиентов по..."), nl,
    write("1 — Названию ингредиента"), nl,
    write("2 — Типу ингредиента"), nl,
    write("0 — Назад"), nl,
    write("Ваш выбор: "), nl,
    read(SearchChoice),
    search_ingredient_choice(SearchChoice).

search_ingredient_choice(1) :- 
    write("Введите название ингредиента: "), read(Name),
    search_ingredient_by_name(Name), nl, !.
search_ingredient_choice(2) :- 
    write("Введите тип ингредиента: "), read(Type),
    search_ingredient_by_type(Type), nl, !.
search_ingredient_choice(0) :- run, !.
search_ingredient_choice(_) :- write("Неверный выбор, попробуйте снова."), nl, search_ingredient_menu.