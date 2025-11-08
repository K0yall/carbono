:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/json)).

:- [backend/src/main, backend/src/kb, backend/src/rules, backend/src/ui, backend/src/explain].

% Configuração CORS para permitir requisições do frontend
:- set_setting(http:cors, [*]).

% Endpoints da API
:- http_handler('/api/consultar', handle_consultar, [method(post)]).
:- http_handler('/api/health', handle_health, [method(get)]).

% Health check
handle_health(_Request) :-
    format('Content-type: application/json~n~n'),
    reply_json_dict(_{status: ok, message: 'Sistema de Carbono Online'}).

% Endpoint principal de consulta
handle_consultar(Request) :-
    cors_enable(Request, [methods([post])]),
    http_read_json_dict(Request, Data),
    format('Content-type: application/json~n~n'),
    
    (   catch(
            processar_consulta(Data, Resultado),
            Error,
            (   print_message(error, Error),
                Resultado = _{error: 'Erro interno no servidor'}
            )
        )
    ->  true
    ;   Resultado = _{error: 'Falha ao processar consulta'}
    ),
    reply_json_dict(Resultado).

% Processa os dados recebidos do frontend
processar_consulta(Data, Resultado) :-
    % Limpar base de fatos dinâmicos
    limpar_base,

    % Extrair dados do JSON
    Setores = Data.setores,
    Meta = Data.meta,
    Compensacao = Data.compensacao,
    CreditosExistentes = Data.creditos_existentes,

    % Inserir dados na base
    assert_setores(Setores),
    assert_meta(Meta),
    assert_compensacao(Compensacao),
    assert_creditos_existentes(CreditosExistentes),

    % Executar regras
    calcular_resultado,

    % Coletar resultados
    resultado_final(ResultadoFinal),
    emissao_total(EmissaoTotal),
    compensacao_existente(_, CompensacaoTotal),

    % Explicação
    findall(Regra, regra_acionada(_, Regra), Regras),

    % Construir resposta JSON
    Resultado = _{
        emissao_total: EmissaoTotal,
        compensacao_total: CompensacaoTotal,
        resultado: ResultadoFinal,
        explicacao: Regras,
        status: success
    }.

% Assert dos setores
assert_setores([]).
assert_setores([Setor|Resto]) :-
    Setor = _{nome: Nome, faturamento: Faturamento},
    calcular_emissao_setor(Nome, Faturamento, Emissao),
    assertz(emissao_setor(Nome, Emissao)),
    assert_setores(Resto).

% Assert da meta
assert_meta(Meta) :-
    number(Meta),
    Meta >= 0, Meta =< 1,
    assertz(meta_reducao(Meta)).

% Assert da compensação
assert_compensacao(Compensacao) :-
    Compensacao = _{tipo: Tipo, quantidade: Quantidade},
    number(Quantidade), Quantidade >= 0,
    assertz(compensacao_existente(Tipo, Quantidade)).

% Assert dos créditos existentes
assert_creditos_existentes(Creditos) :-
    number(Creditos), Creditos >= 0,
    assertz(creditos_disponiveis(Creditos)).

% Inicialização do servidor
:- initialization
    (   current_prolog_flag(argv, [PortString|_])
    ->  atom_number(PortString, Port)
    ;   Port = 8000
    ),
    format('Iniciando servidor na porta ~w...~n', [Port]),
    format('Acesse: http://localhost:~w~n', [Port]),
    server(Port).

% Inicia servidor HTTP
server(Port) :-
    http_server(http_dispatch, [port(Port)]).