1. Необходимо создать бота, который может проводить голосования в Telegram
2. Бот должен обрабатывать команды(сообщения) пользователей и отдавать ответ (запрос-ответ)

3 Команды пользователей

3.0 Формат описания команд
	/строка_с_подчеркиваниям - команда, далее ее параметры, возможно многострочные(то есть строк может быть несколько, новая строка имеет значение!)
	
	Параметры:
	
		<> - обязательный элемент
		
		[] - необязательный элемент, если необязательных параметров несколько - для указания значения параметра необходимо указать все которые идут до него (нельзя указать второй параметр и оставить первый дефолтным)

		Отдельные параметры выделяются с помощью (), если же в теле параметра попадается '(' или ')',
		то необходимо экранировать такой знак с помощью удвоения.

		Внутри <> [] имеют следующий формат
			<значение параметра:тип параметра>
			Тип параметра может быть:
				digits - некое положительное целое число
				string - строка
				date   - дата время в формате hh:mm:ss yy:MM:dd
				перечисление вариантов через | (Пример <пример:foo|bar|baz>), причем для необязательного параметра первый вариант является вариантом по умолчанию.


	Пример описания:
		Создание вопроса. Для вопроса типа "open" варианты ответа будут проигнорированы.
		 /create_question <вопрос:string> [тип вопроса:open|choice|multi]
		 <Вариант ответа 1>
		 <Вариант ответа 2>
		 <Вариант ответа 3>
	Пример вызова:
		 /create_question (Куда идем завтра((пятница))?) (choice)
		 В бар!
		 В кино!
		 Сидим в офисе


3.1 Общие команды
	
	3.1.1 Создание голосования
		Команда
			/create_vote <название_голосвания:string> [анонимность:yes|no] [видимость результата:afterstop|continuous] [время начала:date] [время окончания:date]
		Ответ бота
			уникальный цифровой идентификатор голосования

		Пояснения:
			- создать голосование может любой пользователь голосования, но управлять им - только создавший
			- анонимность заключается в том, что не сохраняются ответы пользователей (только факт голосования и результат)
			- видимость результатов "afterstop" - результаты голосования можно посмотреть только после окончания, "continuous" - в процессе
			- если время начала голосования задано, то бот авоматически стартует голосование в это время и вручную его стартовать нельзя
			- если время конца голосования задано, то бот авоматически заершает голосование в это время и вручную его завершить нельзя
			- голосовать можно только за активные голосования (то есть стартованные и не завершенные)
			- после старта голосования изменять его нельзя!

	
    3.1.2 Список голосований
		Команда
			/list

		Ответ бота
			Список голосований с идентификаторами (как минимум)

	3.1.3 Удаление голосования
		Команда
			 /delete_vote <номер голосования:digits>

		Ответ бота
			сообщение об успехе или ошибка

	3.1.4 Старт процесса голосвания
		Команда
			 /start_vote <номер голосования:digits>

		Ответ бота
			сообщение об успехе или ошибка

	3.1.5 Стоп процесса голосвания
		Команда
			 /stop_vote <номер голосования:digits>

		Ответ бота
			сообщение об успехе или ошибка


	3.1.6 Посмотреть результаты голосования
		Команда
			/result <id голосования:digits>

		Ответ бота
			Красивый отчет о текущем/прошедшем голосовании

3.2 Команды с контекстом

	3.2.1 Начать работу с голосованием (переключиться в контекст)
		Команда
			/begin <id голосования:digits>
		Ответ бота
			сообщение об успехе или ошибка

	3.2.2 Закончить работу с голосованием (отключиться от контекста)
		Команда
			/end
		Ответ бота
			сообщение об успехе или ошибка


	3.2.3 Посмотреть голосование (требует выбранного голосования)
		Команда
			/view

		Ответ бота
			Красивое представление голосования

	3.2.4 Добавление вопроса в голосование (требует выбранного голосования)
		Команда
			 /create_question <вопрос:string> [тип вопроса:open|choice|multi]
			 <Вариант ответа 1>
			 <Вариант ответа 2>
			 <Вариант ответа 3>
			 ...

		Ответ бота
			номер вопроса ПП в рамках голосования либо понятная ошибка

		Пояснения
			- открытый тип вопроса - вписать любую строку. при подведении итогов будут выданы все результаты
			- choice - выбор одного варианта. при подведении итогов будет выданы гистограмма по вариантам.
			- multi - множественный выбор. при подведении итогов будет выданы гистограмма по вариантам.


	3.2.5 Удаление вопроса из голосования (требует выбранного голосования)
		Команда
			 /delete_question <номер вопроса ПП:digits>

		Ответ бота
			сообщение об успехе или ошибка

	3.2.6 Проголосовать (требует выбранного голосования)
		Команда
			/vote <номер вопроса ПП:digits> <ответ>

		Ответ бота
			сообщение об успехе или ошибка

		Пояснения
			- формат ответа:
				open: <ответ на вопрос:string>
				choice: <номер ответа:digit>
				multi: <номера ответов:digit digit digit ... >

				естественно номер ответа должен существовать, в случае mulit номер не должен повторяться

			- один пользователь может отвечать на один вопрос один раз
