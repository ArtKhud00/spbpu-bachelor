# Знакомство с Raspberry Pi
Здесь представлен код программы, осуществляющей передачу данных между микрокомпьютерами Raspberry Pi, работающей в одной сети. Полагается, что микрокомпьютеры решают совместную задачу и в некоторой степени реализуют функционал мультиагентной системы. Код соответствует устройству `2` уровня.
## Особенности реализации
 - В качестве протокола передачи данных используется `UDP`
 - Передача осуществляется с использованием `сокетов`
 - По завершении работы программы сокеты закрываются
## Описание идеи
Для реализации функционала мультиагентной системы была выбрана задача зажигания диодов. Были использованы три Raspberry Pi, образующих иерархическую мультиагентную систему. Самая верхняя по иерархии Raspberry Pi отправляет команду на включение какого – то конкретного светодиода. Следующая Raspberry Pi получает данную команду и отправляет запрос нижестоящей Raspberry Pi может ли она включить данный светодиод. Нижнеуровневая Raspberry Pi принимает этот запрос и проводит проверку может ли она включить нужный диод, после чего возможны 2 сценария: 
  1) Требуемый светодиод не включен, и данная Raspberry Pi отправляет сообщение вышестоящему микрокомпьютеру о готовности его включить. Raspberry Pi, стоящая на втором уровне получает данное сообщение и отправляет команду устройству на третьем уровне о включении светодиода. Получив данную команду, устройство третьего уровня включает диод и отправляет сообщение об успешном включении диода Raspberry Pi на 2 уровне. Получив данное сообщение, микрокомпьютер второго уровня передает сообщение вышестоящей Raspberry Pi об успешном выполнении требуемой команды;
  2) Требуемый светодиод уже включен, и Raspberry Pi на 3 уровне посылает сообщение о том что диод уже включен. Получив данное сообщение, микрокомпьютер второго уровня зажигает на время свой светодиод, тем самым сигнализируя об ошибке и отправляет сообщение вышестоящей Raspberry Pi о том, что команда не была выполнена. 