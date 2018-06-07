(setq net '((	A  	B 	C  	D	E	F)
		    ((A 0) 	00	2	4 	8 	0	16)
		    ((B 0)	2	00	0	3	0	0 )
		    ((C 0)	4	0	00	3	0	0 )
		    ((D 0)	8	3	3	00	5	3 )
		    ((E 0)	0	0	0	5	00	5 )
		    ((F 0)	16	0	0	3	5	00)
		)
)

(setq infinity 1000) ; �������� infinity =  max (net) + 1

(defun createListOfUnxploredNodes (net start) 
"������� ������� ������ �������������� ������ ����:((A <infinity> 0 0) (B <infinity> 0 0) .. )"
	(cond (
		(eq (car net) nil) 
		;���� ����������� ��� ������� � ����, �� ������� �� ��������
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
"���������� ������ nodesList ��� �������� nodeToDelete, �� ����������� �������� ������."
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
"������� ������� ������ ����������� � �������� nodeToProcess �� net, ���� ������� � net ���� � avaibleNodes"
	(cond
		(
			(eq (caaadr net) nodeToProcess) 
			;���� ����� ������� � ������ ������, ��
				(   ;������������ ��� ������ � ��������� ������ � ���� ���������� ������ � ������� �� 0, �� ��������� �� � ������ �������� ���������
					extendResult (exploreConnectionsOfNode (cdadr net) listOfAllNodes) 
					avaibleNodes
				)

		)
		(T	;���� �� �����, �� ������������� ��������� ������
			(findConnectedNodes (cdr net) nodeToProcess listOfAllNodes avaibleNodes)
		)
	)
)

(defun exploreConnectionsOfNode (lineToExplore listOfAllNodes) 
 "������� ��������� ���� ������ � ��������� ������ � ���� ���������� ������ � ������� �� 0, �� ��������� �� � ������ �������� ���������"
	(cond
		(
			(eq (car lineToExplore) nil) 
			;���� ����������� ���� ������, �� ������� �� ��������
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
	"��������� ���������� ������ ������ ����: ((A 3) (B 7)) �� ����: ((A 1000 ((0)) 3) (B 1000 ((0)) 7))"
	(cond												
	;��� ���� ������������ avaibleNodes, ���� �����-�� ������� ��� ���, �� ��� �� ������� � �������������� ������
		(
			(eq (car listOfConnectedNodes) nil) 
			;���� ����������� ���� ������, �� ������� �� ��������
				nil
		)
		(T
			(cond
				(
					(eq
						(setq 
							obtainedList 
							;������� � ������ ��������� ������ ������ ������� �������
							(findListWithNodeInHead (caar listOfConnectedNodes) avaibleNodes )
						)  
						nil
					)
						(cond
							(
								(eq (car listOfConnectedNodes) nil) 
								;���� ����������� ���� ������, �� ������� �� ��������
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
	"������� ������������� ������ ����������� ������ � ���� �������� ����������, ���� ��� ������, ���� ��������� ��� ��� ����"
	(cond
	;��� ���� ������������ avaibleNodes, ���� �����-�� ������� ��� ���, �� ��� �� ������� � �������������� ������
		(
			(eq (setq processingNode (car connectedNodes)) nil) 
			;���� ����������� ���� ������, �� ������� �� ��������
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
			(car observingNode) ;�������� ��� ������� observingNode
			nil
		)
		(cons
			(+ 
			;���������� distance observingNode � value currentNode � � ����� �������� �� observingNode � �������� ��� ��� � distance observingNode
				(cadddr observingNode)
				(+
					(cadr currentNode)
					(findTransferCost net observingNode) 
					;������� ������� "�����" ��������� �� ������� observingNode
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
			"����� �������� ���������:"
			nil
		)
		(append
			(cons
				(cadr outPut)
				nil
			)
			(append
				(cons
					"� ����� �������� ����:"
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
 "�������� ������� ������: ������, ��������� �����, �������� �����, ����������������� ������ � ��������� ���� <infinity>"
	(cond
		((eq start goal)(createOuputMessage outPut))
		(T (lookingFor	net
				(car
					(setq outPut (
						findNodeWithSmallestValue  
						;������� ������� ������� � ���������� value � ���������� �� � ���� (D 10 ((B C) (A F)) X)
						;������� replaceValueAndListOfNodesIfNeeded ������ ������ ������, ����� ������� ������� ������� � ����������� value
						(setq listOfUnexploredNodes	
							(replaceValueAndListOfNodesIfNeeded	
							;1 �������� - findDistanceToConnectedNodes
							;findDistanceToConnectedNodes ������ ������ ������ � ���������� �� ���. ����� ����� ����������� ���� ������ � ������ �������������� ������ � ��� ���� ���������� ����� ��������.
								(findDistanceToConnectedNodes
									;������ �������� - �������� �����, ��� ����������� �������� ��� ���������� �� ��������
									net 
									;������ ��������  - ����������� �������
									(findConnectedNodes 
										;������� ��������� ������� � �������
										net  
										(cond (
												(eq listOfUnexploredNodes nil)
												(car (setq currentNode 
													;�������� ������� ������� � ���� (A 0 (0) 0)
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
													;�������� ������� ������� � ���� (A 0 (0) 0)
													(car(extendResult  (cons(append (cons start nil) `(0)) nil) listOfUnexploredNodes))
													)
											    )
											)
										)
										(car net)
										(cond 	( 
													(eq listOfUnexploredNodes nil)
													(setq listOfUnexploredNodes	
													;��������� ���������� ������ � unexloredNodes
														(deleteVisitedNode 	
														;deleteVisitedNode ������ ������ ��������������� ������ ��� ����������� ���������
															(setq unexploredNodes (createListOfUnxploredNodes net start)) 
															(car currentNode)
														)
													)
												)
												(T 	(setq listOfUnexploredNodes	
													;��������� ���������� ������ � unexloredNodes
														(deleteVisitedNode 	listOfUnexploredNodes 
														;deleteVisitedNode ������ ������ ��������������� ������ ��� ����������� ���������
															(car currentNode)
														)
													)
												)
										)
									) ; ��� ������ ��������� findConnectedNodes 
									currentNode ;������ - ������� ��������������� �������
								) ; ��� ������ ��������� findDistanceToConnectedNodes
								;2�� - ������ ������������ ������
								listOfUnexploredNodes
								;3� - ������ ������ ������� ������� + ��� ������� �������
								(createOutputListOfNodesForCurrentNode 	
								;� ������� ������� B ���� ������ ����: ((A E) (C D)), ������� ���������� ������ ���� ((A E B) (C D B))
								;���� ������ ������� � ������� ������� ����� ���: ((A E)), �� ������� ������ ������ ((A E B))
								;���� ������ ������ � ������� ������� ����� ���: (0), �� ������� ������ ((B))
									(car currentNode) ;1 �������� ��� �������
									(caddr currentNode) ;2 �������� ������ ������
								)
							) ; ��� ������ ��������� replaceValueAndListOfNodesIfNeeded
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
"������� ������� � ���������� ���������"
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
"���������� 2 �������"
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
	;��� ����� �� ����� listOfConnectedNode ������ ����: ((B 7) (D 14) ..)
	;������� � ������ listOfUnexploredNodes ������� � ������� ���������, �������� (D 1000 0 0)
	;� ���� ������ �������� (1000) > ������� ��������� 1��� ������ (14), �� �������� 1000 �� 14
	;� �������� ������ ������ �� ������� �������
	;������ listOfConnectedNode ((B 4) (D 7))
	;������ listOfUnexploredNodes ((B 1000 (0) 0) (C 1000 (0) 0) (D 1000 (0) 0) (E 1000 (0) 0))
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
						nil ;���� � ����������� ��������, ����� ���
					)
						(cons	;�� ��������� ������� �� ������������� ������� ��� ���������
							(car listOfUnexploredNodes)
							(replaceValueAndListOfNodesIfNeeded listOfConnectedNode (cdr listOfUnexploredNodes) listOfNodeToReplacePath)
						)
				)
				(T	;�������� ����, ���� ����� � ������ listOfConnectedNode ������ �������
					(cond	;���������� �������� X �� (D X) � Y �� ((D Y 0 0))
						(
							(<	;���� X < Y, �� �������� Y �� X �
								(cadr foundedNode)
								(cadar listOfUnexploredNodes)
							)
								(cons
									(append
										(append
											(append
												(cons
													(car foundedNode) ;��� �������
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
							(=	;���� X = Y, �� X �� ��������, � � ������ ������ ��������� listOfNodeToReplacePath, ������� � �������
								;((A B C D) (A C E)..)
								(cadr foundedNode)
								(cadar listOfUnexploredNodes)
							)
								(cons
									(append
										(append
											(append
												(cons
													(car foundedNode) ;��� �������
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
							;�������� ����, ���� X > Y, � ���� ������ ��������� ��� ��� ���������
							(cons
								(append
									(append
										(append
											(cons
												(caar listOfUnexploredNodes) ;��� �������
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

