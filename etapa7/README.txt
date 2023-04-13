Projeto de compiladores - Etapa 7 - Grafo de controle de fluxo
Grupo H - Aluno: Guilherme Dytz dos Santos - Cartão: 244475

- Exemplos
    Existem 2 exemplos de código no diretório examples:
        - ex1: código na linguagem usada ao longo do semestre que essencialmente contém
               um if-then-else que atribui valores diferentes a uma variável baseado no
               teste do if
        - ex2: código na linguagem usada ao longo do semestre que possui um comando while
               aninhado que incrementa dias variáveis até que as condições sejam satisfeitas

- Como rodar os exemplos:
    Foram adicionadas duas novas regras no Makefile do projeto: ex1 e ex2.
    Essas duas regras essencialmente fazem a build do código e geram um binário que, ao ser
    executável, gera os .dot com os grafos de controle de fluxo das instruções iloc geradas
    a partir de cada exemplo. Por exemplo, ao rodar o comando:

      make ex1

    Irão surgir na raíz do projeto 3 arquivos: ex1.iloc, ex1.dot e ex1.png. Sendo que o arquivo
    .iloc contém as instruções geradas a partir do exemplo 1, o .dot contém a descrição not formato
    DOT do grafo de controle de fluxo gerado a partir dessas instruções e o png contém a representação
    gráfica desse grafo.

    De forma análoga, é possível gerar os mesmos arquivos para o exemplo 2 com o comando "make ex2".

- Código implementado:
    Toda a implementação que gera o código iloc é essencialmente a mesma feita na Etapa 5, sendo feito
    apenas correções de bugs e melhorias no código.
    A implementação onde é gerado o grafo está presente no arquivo "graph.rs" do diretório src. Nele
    estão implementados:
        - Estrutura FluxCtrlGraphNode: estrutura que mantém controle dos dados pertinentes a um nó
        do grafo de controle de fluxo. Nela fica armazenado essencialmente o código do bloco básico
        que o nó representa, assim como os nós filhos (nós para quais o bloco básico possui uma aresta),
        além do seu id para facilitar a geração do arquivo DOT. Essa estrutura possui dois métodos auxiliares,
        um que adiciona um novo filho ao seu vetor de filhos e outro que, dado um label, verifica se esse nó
        possui esse label associado a ele.

        - Função find_leaders: dado um vetor com o código iloc, retorna a lista de posições de todos os líderes
        de bloco básico de acordo com o algoritmo que foi visto em aula.

        - Função get_basic_blocks: dado um vetor de líderes e o vetor de instruções iloc, essa função elenca a
        posição de início e fim de cada bloco básico do código.

        - Função build_graph: dado um vetor de intruções iloc, essa função usa os auxiliares elencados anteriormente
        para gerar o grafo de controle de fluxo, que é essencialmente um vetor com todos os nós gerados a partir dos
        blocos básicos. Além disso, essa função verifica quais nós possuem arestas entre si verificando as instruções
        de branch/jump e os fall throughs apropriados.

        - Função print_graph: como o nome sugere, ela printa o grafo na saída padrão utilizando o padrão DOT.

    Além disso, foi implementado um método auxiliar para a estrutura que armazena a instrução iloc: get_jump_labels.
    Esse método retorna um vetor de labels para qual a instrução faz jump caso seja uma instrução de jump, caso
    contrário, esse método retorna um vetor vazio.

- Considerações assumidas na implementação:
    Para facilitar a implementação, assumiu-se que a unica função presente no código passado para o binário é a main,
    dado que seria necessário calcular o valor presente em um registrador para computar o endereço certo dos jumps de
    retorno de função a partir dos registradores. Como a implementação assume que a única função do código de entrada é
    a main, haverá apenas um jump associado a um registrado, sendo assim, o jump já é computado para a instrução de halt
    presente no trecho que "chama" a função main.
    Com isso, a implementação não irá gerar um grafo de controle de fluxo correto caso haja mais de uma função
    implementada  no código de entrada.