(setq net '((	A  	B 	C  	D	E	F)
		    ((A 0) 	00	2	4 	8 	0	16)
		    ((B 0)	2	00	0	3	0	0 )
		    ((C 0)	4	0	00	3	0	0 )
		    ((D 0)	8	3	3	00	5	3 )
		    ((E 0)	0	0	0	5	00	5 )
		    ((F 0)	16	0	0	3	5	00)
		)
)

(setq infinity 1000) ; Багофикс infinity =  max (net) + 1

(defun createListOfUnxploredNodes (net start) 
"Функция создает список неиследованных вершин вида:((A <infinity> 0 0) (B <infinity> 0 0) .. )"
	(cond (
		(eq (car net) nil) 
		;Если просмотрели все вершины в сети, то выходим из рекурсии
		nil
	)
	(T
		(cons
		(append
			(append
			(append
				(cons (append nil (caar net)) nil)
				(cond
				(
					(eq(caar net) start)
					(cons
						`0
						nil
					)
				)
				(T
					(cons
									infinity
										nil
									)
								)
							)
						)
						`(((0)))
					)
					`(0)
				)
				(createListOfUnxploredNodes
					(cons
						(cdar net)
						nil
					)
					start
				)
			)
		)
	)
)

(defun deleteVisitedNode(nodesList nodeToDelete)	
"Возвращает список nodesList без элемента nodeToDelete, не модифицируя исходный список."
	(cond
		(
			(eq(caar nodesList) nodeToDelete)
				(cdr nodesList)
		)
		(T
			(cons
				(car nodesList)
				(deleteVisitedNode (cdr nodesList) nodeToDelete)
			)
		)
	)
)


(defun findConnectedNodes (net nodeToProcess listOfAllNodes avaibleNodes) 
"Функция выводит список соединненых с вершиной nodeToProcess из net, если вершина в net есть в avaibleNodes"
	(cond
		(
			(eq (caaadr net) nodeToProcess) 
			;Если нашли вершину в нужной строке, то
				(   ;Просмариваем все ячейки в найденной строке и если втретиться ячейка в которой не 0, то добавляем ее в список выходных праметров
					extendResult (exploreConnectionsOfNode (cdadr net) listOfAllNodes) 
					avaibleNodes
				)

		)
		(T	;Если не нашли, то просматриваем следующую строку
			(findConnectedNodes (cdr net) nodeToProcess listOfAllNodes avaibleNodes)
		)
	)
)

(defun exploreConnectionsOfNode (lineToExplore listOfAllNodes) 
 "Функция просмотра всех ячейки в найденной строке и если втретиться ячейка в которой не 0, то добавляем ее в список выходных праметров"
	(cond
		(
			(eq (car lineToExplore) nil) 
			;Если просмотрели весь список, то выходим из рекурсии
			nil
		)
		(T
			(cond
				(
					(> (car lineToExplore) 0)
						(cons
							(append
								(cons(car listOfAllNodes) nil)
								(cons(car lineToExplore) nil)
							)
							(exploreConnectionsOfNode (cdr lineToExplore) (cdr listOfAllNodes))
						)
				)
				(T
					(exploreConnectionsOfNode (cdr lineToExplore) (cdr listOfAllNodes))
				)
			)
		)
	)

)

(defun findListWithNodeInHead (searchingNode nodesList)
	(cond
		(
			(eq (car nodesList) nil)
				nil
		)
		(T
			(cond
				(
					(eq(caar nodesList) searchingNode)
						(car nodesList)
				)
				(T
					(findListWithNodeInHead searchingNode (cdr nodesList))
				)
			)
		)
	)
)

(defun extendResult(listOfConnectedNodes avaibleNodes) 
	"Расширяет полученный список вершин вида: ((A 3) (B 7)) до вида: ((A 1000 ((0)) 3) (B 1000 ((0)) 7))"
	(cond												
	;При этом просматривая avaibleNodes, если какой-то вершины там нет, то она не попадет в результирующий список
		(
			(eq (car listOfConnectedNodes) nil) 
			;Если просмотрели весь список, то выходим из рекурсии
				nil
		)
		(T
			(cond
				(
					(eq
						(setq 
							obtainedList 
							;Находит в списке доступных вершин список искомой вершины
							(findListWithNodeInHead (caar listOfConnectedNodes) avaibleNodes )
						)  
						nil
					)
						(cond
							(
								(eq (car listOfConnectedNodes) nil) 
								;Если просмотрели весь список, то выходим из рекурсии
									nil
							)
							(T
								(extendResult (cdr listOfConnectedNodes) avaibleNodes)
							)
						)
				)
				(T
					(append
						(cons
							(append
								(append
									(append
										(cons
											(car obtainedList)
											nil
										)
										(cons
											(cadr obtainedList)
											nil
										)
									)
									(cons
										(caddr obtainedList)
										nil
									)
								)
								(cons
									(+
										(cadddr obtainedList)
										(cadar listOfConnectedNodes)
									)
									nil
								)
							)
							nil
						)
						(extendResult (cdr listOfConnectedNodes) avaibleNodes)
					)
				)
			)
		)
	)
)




(defun findDistanceToConnectedNodes (net connectedNodes currentNode)
	"Функция просматриваем список соединненых вершин и либо заменяет расстояние, если оно меньше, либо оставляет так как есть"
	(cond
	;При этом просматривая avaibleNodes, если какой-то вершины там нет, то она не попадет в результирующий список
		(
			(eq (setq processingNode (car connectedNodes)) nil) 
			;Если просмотрели весь список, то выходим из рекурсии
				nil
		)
		(T
			(cons
				(findDistanceToNode net currentNode processingNode)
				(findDistanceToConnectedNodes net (cdr connectedNodes) currentNode)
			)
		)
	)
)


(defun findDistanceToNode (net currentNode observingNode)
	(append
		(cons
			(car observingNode) ;Получаем имя вершины observingNode
			nil
		)
		(cons
			(+ 
			;Складываем distance observingNode с value currentNode и с весом перехода на observingNode и помещаем все это в distance observingNode
				(cadddr observingNode)
				(+
					(cadr currentNode)
					(findTransferCost net observingNode) 
					;Находим сколько "весит" пересадка на станции observingNode
				)
			)
			nil
		)
	)
)

(defun findTransferCost (net observingNode)
	(cond
		(
			(eq (caaadr net) (car observingNode))
				(car(cdaadr net))
		)
		(T
			(findTransferCost (cdr net) observingNode)
		)
	)
)
(defun createOuputMessage (outPut)
	(append
		(cons
			"Самая короткая дистанция:"
			nil
		)
		(append
			(cons
				(cadr outPut)
				nil
			)
			(append
				(cons
					"и самые короткие пути:"
					nil
				)
				(addGoalToListOfWays (car outPut) (caddr outPut))
			)
		)
	)
)
(defun addGoalToListOfWays (finalNode listOfWays)
	(cond
		(
			(eq (car listOfWays) nil)
			nil
		)
		(T
			(cons
				(append
					(car  listOfWays)
					(cons finalNode nil)
				)
				(addGoalToListOfWays finalNode (cdr listOfWays))
			)
		)
	)
)

(defun lookingFor (net start goal &optional listOfUnexploredNodes )
 "Основная функция поиска: массив, начальная точка, финишная точка, отформатированный список с вершинами типа <infinity>"
	(cond
		((eq start goal)(createOuputMessage outPut))
		(T (lookingFor	net
				(car
					(setq outPut (
						findNodeWithSmallestValue  
						;Функция находит вершусу с минимльным value и возвращает ее в виде (D 10 ((B C) (A F)) X)
						;Функция replaceValueAndListOfNodesIfNeeded вернет список вершин, среди которых находим вершину с минимальным value
						(setq listOfUnexploredNodes	
							(replaceValueAndListOfNodesIfNeeded	
							;1 аргемент - findDistanceToConnectedNodes
							;findDistanceToConnectedNodes Вернет список вершин и расстояния до них. Далее нужно просмотреть этот список и список неиследованных вершин и где надо установить новое значение.
								(findDistanceToConnectedNodes
									;Первый аргумент - исходная схема, для определения задержек при пересадках на станциях
									net 
									;Второй аргумент  - соединенные вершины
									(findConnectedNodes 
										;Находим связанные вершины с текущей
										net  
										(cond (
												(eq listOfUnexploredNodes nil)
												(car (setq currentNode 
													;Получили текущую вершину в виде (A 0 (0) 0)
														(car (extendResult  
																(cons(append (cons start nil) `(0)) nil)
																(setq unexploredNodes (createListOfUnxploredNodes net start))
															)
														)
													)
												)
											)
											(T (car
													(setq currentNode 
													;Получили текущую вершину в виде (A 0 (0) 0)
													(car(extendResult  (cons(append (cons start nil) `(0)) nil) listOfUnexploredNodes))
													)
											    )
											)
										)
										(car net)
										(cond 	( 
													(eq listOfUnexploredNodes nil)
													(setq listOfUnexploredNodes	
													;Сохраняем полученный списко в unexloredNodes
														(deleteVisitedNode 	
														;deleteVisitedNode вернет список неисследованных вершин без модификации исходного
															(setq unexploredNodes (createListOfUnxploredNodes net start)) 
															(car currentNode)
														)
													)
												)
												(T 	(setq listOfUnexploredNodes	
													;Сохраняем полученный списко в unexloredNodes
														(deleteVisitedNode 	listOfUnexploredNodes 
														;deleteVisitedNode вернет список неисследованных вершин без модификации исходного
															(car currentNode)
														)
													)
												)
										)
									) ; Эта скобка закрывает findConnectedNodes 
									currentNode ;Третий - текущая рассматриваемая вершина
								) ; Эта скобка закрывает findDistanceToConnectedNodes
								;2ой - список непосещенных вершин
								listOfUnexploredNodes
								;3й - список вершин текущей вершины + имя текущей вершины
								(createOutputListOfNodesForCurrentNode 	
								;У текущей вершины B есть список вида: ((A E) (C D)), функция возвращает список вида ((A E B) (C D B))
								;Если список вершщин у текущей вершины имеет вид: ((A E)), то функция вернет список ((A E B))
								;Если список вершин у текущей вершины имеет вид: (0), то функция вернет ((B))
									(car currentNode) ;1 аргумент имя вершины
									(caddr currentNode) ;2 аргумент список вершин
								)
							) ; Эта скобка закрывает replaceValueAndListOfNodesIfNeeded
						)
					)
				)
			)
			goal
			listOfUnexploredNodes
			)
		)
	)
)

(defun findNodeWithSmallestValue (lisOfNodes &optional currentMin)
"Находит вершину с наименьшим значением"
	(cond
		(
			(eq (car lisOfNodes) nil)
				currentMin
		)
		(
			(eq currentMin nil)
				(findNodeWithSmallestValue	
					(cdr lisOfNodes)
					(setq currentMin (car lisOfNodes))
				)
		)
		(T
			(findNodeWithSmallestValue		(cdr lisOfNodes)
											(minOfTwoNodes currentMin (car lisOfNodes))
			)
		)
	)
)

(defun minOfTwoNodes (node1 node2)
"Сравнивает 2 вершины"
	(cond
		(
			(<
				(cadr node1)
				(cadr node2)
			)
				node1
		)
		(T
			node2
		)
	)
)

(defun createOutputListOfNodesForCurrentNode (nodeName listOfNodeForCurrentNode)
	(cond
		(
			(eq (car listOfNodeForCurrentNode) nil)
				nil
		)
		(T
			(cons
				(cond
					(
						(eq (caar listOfNodeForCurrentNode) `0)
							(cons
								nodeName
								nil
							)
					)
					(T
						(append
							(car listOfNodeForCurrentNode)
							(cons
								nodeName
								nil
							)
						)
					)
				)
				(createOutputListOfNodesForCurrentNode nodeName (cdr listOfNodeForCurrentNode))
			)
		)
	)
)

(defun getConnectedNodeByCurrentUnxploredNode (nodeToSeach listOfNodes)
	(cond
		(
			(eq (car listOfNodes) nil)
				nil
 		)
		(T
			(cond
				(
					(eq (caar listOfNodes) nodeToSeach)
						(car listOfNodes)
				)
				(T
					(getConnectedNodeByCurrentUnxploredNode nodeToSeach (cdr listOfNodes))
				)
			)
		)
	)
)

(defun replaceValueAndListOfNodesIfNeeded (listOfConnectedNode listOfUnexploredNodes listOfNodeToReplacePath)
	;Тут имеем на месте listOfConnectedNode список вида: ((B 7) (D 14) ..)
	;Находим в списке listOfUnexploredNodes вершину с похожим названием, например (D 1000 0 0)
	;И если второй параметр (1000) > второго параметра 1ого списка (14), то заменяем 1000 на 14
	;и заменяем список вершин от текущей вершины
	;Формат listOfConnectedNode ((B 4) (D 7))
	;Формат listOfUnexploredNodes ((B 1000 (0) 0) (C 1000 (0) 0) (D 1000 (0) 0) (E 1000 (0) 0))
	(cond
		(
			(eq (car listOfUnexploredNodes) nil)
			nil
		)
		(T
			(cond
				(
					(eq(setq foundedNode 	(getConnectedNodeByCurrentUnxploredNode (caar listOfUnexploredNodes)
																					listOfConnectedNode
											)
						)
						nil ;Если в соединенных вершинах, такой нет
					)
						(cons	;то оставляем текущую не исследованную вершину без изменения
							(car listOfUnexploredNodes)
							(replaceValueAndListOfNodesIfNeeded listOfConnectedNode (cdr listOfUnexploredNodes) listOfNodeToReplacePath)
						)
				)
				(T	;Попадаем сюда, если нашли в списке listOfConnectedNode нужную вершину
					(cond	;Сравниваем значение X из (D X) и Y из ((D Y 0 0))
						(
							(<	;Если X < Y, то заменяем Y на X и
								(cadr foundedNode)
								(cadar listOfUnexploredNodes)
							)
								(cons
									(append
										(append
											(append
												(cons
													(car foundedNode) ;Имя вершины
													nil
												)
												(cons
													(cadr foundedNode) ;X
													nil
												)
											)
											(cons
												listOfNodeToReplacePath
												nil
											)

										)
										(cdddar listOfUnexploredNodes)
									)
									(replaceValueAndListOfNodesIfNeeded (cdr listOfConnectedNode)
																		(cdr listOfUnexploredNodes)
																		listOfNodeToReplacePath
									)
								)
						)
						(
							(=	;Если X = Y, то X не изменяем, а в список вершин добавляем listOfNodeToReplacePath, который в формате
								;((A B C D) (A C E)..)
								(cadr foundedNode)
								(cadar listOfUnexploredNodes)
							)
								(cons
									(append
										(append
											(append
												(cons
													(car foundedNode) ;Имя вершины
													nil
												)
												(cons
													(cadar listOfUnexploredNodes) ;X
													nil
												)
											)
											(cons
												(append
													(caddar listOfUnexploredNodes)
													listOfNodeToReplacePath

												)
												nil
											)

										)
										(cdddar listOfUnexploredNodes)
									)
									(replaceValueAndListOfNodesIfNeeded 
										(cdr listOfConnectedNode)
										(cdr listOfUnexploredNodes)
										listOfNodeToReplacePath
									)
								)
						)
						(T
							;Попадаем сюда, если X > Y, в этом случае оставляем все без изменения
							(cons
								(append
									(append
										(append
											(cons
												(caar listOfUnexploredNodes) ;Имя вершины
												nil
											)
											(cons
												(cadar listOfUnexploredNodes) ;X
												nil
											)
										)
										(cons
											(caddar listOfUnexploredNodes)
											nil
										)
									)
									(cdddar listOfUnexploredNodes)
								)
								(replaceValueAndListOfNodesIfNeeded 
									(cdr listOfConnectedNode)
									(cdr listOfUnexploredNodes)
									listOfNodeToReplacePath
								)
							)
						)
					)
				)
			)
		)
	)
)

(print (lookingFor net `A `F))

