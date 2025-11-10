/*
 * ui.pl - Interface de Usuario
 * Funcoes para interacao via console
 */

% Banner do sistema
mostrar_banner :-
    nl,
    writeln('========================================================'),
    writeln('     SISTEMA ESPECIALISTA - MERCADO DE CARBONO          '),
    writeln('========================================================'),
    writeln('   Analise de Emissoes e Compliance com Metas Climaticas'),
    writeln('========================================================'),
    nl.

% Roda pe com desenvolvedores
mostrar_rodape :-
    nl,
    writeln('--------------------------------------------------------'),
    writeln('Desenvolvido por: Lucas Gilmar da Silva -> @Koyall'),
    writeln('Desenvolvido por: Felipe Jose Sens -> @FelipeJoseSens'),
    writeln('--------------------------------------------------------'),
    nl.

% Menu principal
menu_principal :-
    mostrar_banner,
    writeln('MENU PRINCIPAL:'),
    writeln('1) Executar consulta'),
    writeln('2) Sair'),
    nl,
    write('Escolha uma opcao: '),
    read(Opcao),
    processar_opcao(Opcao).

% Processar opcao do menu
processar_opcao(1) :-
    limpar_base,
    coletar_dados,
    calcular_resultado,
    exibir_resultado,
    explicar_resultado,
    nl,
    write('Pressione ENTER para voltar ao menu...'),
    read(_),
    menu_principal.

processar_opcao(2) :-
    nl,
    writeln('Obrigado por usar o Sistema de Carbono!'),
    mostrar_rodape,
    halt.

processar_opcao(_) :-
    nl,
    writeln('Opcao invalida! Tente novamente.'),
    nl,
    menu_principal.

% Coleta dados do usuario
coletar_dados :-
    nl,
    writeln('=== COLETA DE DADOS DA EMPRESA ==='),
    nl,

    % Emissoes por setor
    writeln('Informe o faturamento anual por setor (em R$):'),
    nl,

    write('Energia: R$ '),
    read_number_validation(FatEnergia),
    calcular_emissao_setor(energia, FatEnergia, EmEnergia),
    assertz(emissao_setor(energia, EmEnergia)),

    write('Transportes: R$ '),
    read_number_validation(FatTransportes),
    calcular_emissao_setor(transportes, FatTransportes, EmTransportes),
    assertz(emissao_setor(transportes, EmTransportes)),

    write('Industria: R$ '),
    read_number_validation(FatIndustria),
    calcular_emissao_setor(industria, FatIndustria, EmIndustria),
    assertz(emissao_setor(industria, EmIndustria)),

    write('Agropecuaria: R$ '),
    read_number_validation(FatAgro),
    calcular_emissao_setor(agropecuaria, FatAgro, EmAgro),
    assertz(emissao_setor(agropecuaria, EmAgro)),

    write('Residuos: R$ '),
    read_number_validation(FatResiduos),
    calcular_emissao_setor(residuos, FatResiduos, EmResiduos),
    assertz(emissao_setor(residuos, EmResiduos)),

    nl,

    % Meta de reducao (Validacao de percentual e divisao)
    write('Meta de reducao de emissoes ate 2030 (%): '),
    read_percentage_validation(MetaPct),
    Meta is MetaPct / 100,
    assertz(meta_reducao(Meta)),

    nl,

    % Compensacao
    write('Possui projetos de compensacao? (sim/nao): '),
    read(TemComp),
    coletar_compensacao(TemComp),

    nl,

    % Creditos existentes
    write('Creditos de carbono disponiveis (tCO2e): '),
    read_number_validation(Creditos), % <--- Validacao aplicada aqui
    assertz(creditos_disponiveis(Creditos)),

    nl.

% Coleta dados de compensacao (Logica mantida, funciona bem para sim/nao)
coletar_compensacao(sim) :-
    write('Tipo de projeto (reflorestamento/energia_renovavel/efficiencia_energetica/captura_carbono): '),
    read(Tipo),
    write('Quantidade compensada anualmente (tCO2e): '),
    read_number_validation(Quantidade), % Garantir que a quantidade e numerica
    assertz(compensacao_existente(Tipo, Quantidade)).

coletar_compensacao(nao) :-
    assertz(compensacao_existente(nenhum, 0)).

coletar_compensacao(_) :-
    writeln('Resposta invalida. Assumindo sem compensacao.'),
    assertz(compensacao_existente(nenhum, 0)).

% Exibe resultado da analise (COM MELHORIAS DE FORMATACAO)
exibir_resultado :-
    nl,
    writeln('========================================================'),
    writeln('              RESULTADO DA ANALISE                      '),
    writeln('========================================================'),
    nl,

    % --- Secao de Totais ---
    writeln('--- Totais de Emissao e Reducao ---'),
    nl,
    emissao_total(EmTotal),
    format('Emissao Total (sem reducao): ~2f tCO2e~n', [EmTotal]),

    calcular_compensacao_total(CompTotal),
    format('Compensacao Existente: ~2f tCO2e~n', [CompTotal]),

    meta_reducao(Meta),
    EmissaoMeta is EmTotal * (1 - Meta),
    format('Meta de Emissao (Apos Reducao de ~0f%%): ~2f tCO2e~n', [Meta * 100, EmissaoMeta]),
    nl,
    
    % --- Secao de Status ---
    writeln('--- Status de Compliance ---'),
    nl,
    resultado_final(R),
    Status = R.status,
    Classificacao = R.classificacao,

    (   Status = compliant
    ->  writeln('STATUS: EM COMPLIANCE 🚀')
    ;   Status = compliant_com_creditos
    ->  writeln('STATUS: EM COMPLIANCE (Com uso de creditos disponiveis) ✅')
    ;   writeln('STATUS: NAO COMPLIANT ❌')
    ),
    nl,
    format('Classificacao da Empresa: ~w~n', [Classificacao]),
    nl,

    % --- Secao de Creditos ---
    writeln('--- Mercado de Carbono ---'),
    nl,
    CreditosNec = R.creditos_necessarios,
    CreditosExc = R.creditos_excedentes,
    Investimento = R.investimento,
    CreditosDisp = R.creditos_disponiveis, % Adicionado para exibir creditos ja usados/disponiveis
    
    format('Creditos de Carbono Disponiveis (Input): ~2f tCO2e~n', [CreditosDisp]),
    nl,

    (   CreditosNec > 0
    ->  format('>> Creditos Adicionais Necessarios: ~2f tCO2e~n', [CreditosNec]),
        format('>> Investimento Estimado em Creditos (Compra): R$ ~2f~n', [Investimento])
    ;   writeln('Sem necessidade de compra de creditos adicionais.')
    ),

    (   CreditosExc > 0
    ->  nl,
        format('>> Creditos Excedentes (Disponiveis para Venda): ~2f tCO2e~n', [CreditosExc])
    ;   true
    ),
    nl,

    % --- Secao de Recomendacao ---
    writeln('--- Recomendacao Estrategica ---'),
    nl,
    Recomendacao = R.recomendacao,
    format('RECOMENDACAO DE INVESTIMENTO: Investir em projeto de ~w~n', [Recomendacao]),
    nl.


% =======================================================
% PREDICADOS DE VALIDACAO DE ENTRADA (MANTIDOS)
% =======================================================

% Le e valida se a entrada e um numero. Repete em caso de erro.
read_number_validation(Number) :-
    read(Input),
    (number(Input) ->
        Number = Input
    ;
        writeln('ERRO: Por favor, insira um valor numerico (ex: 5000.00).'),
        write('Tente novamente: R$ '),
        read_number_validation(Number) % Recursao para repetir a leitura
    ).

% Le e valida a Meta de Reducao (garante que e um numero e avisa sobre valores absurdos)
read_percentage_validation(Percent) :-
    read_number_validation(Percent0),
    (Percent0 >= 0, Percent0 =< 100 ->
        Percent = Percent0
    ; Percent0 > 100 ->
        writeln('AVISO: Metas acima de 100% sao extremamente raras. Usando o valor inserido.'),
        Percent = Percent0
    ;
        writeln('ERRO: A meta de reducao deve ser um percentual positivo. Tente novamente.'),
        write('Meta de reducao (%): '),
        read_percentage_validation(Percent) % Repete a leitura
    ).