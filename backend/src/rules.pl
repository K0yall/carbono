/*
 * rules.pl - Regras de Negocio para Analise de Carbono
 * Define as 9+ regras principais do sistema
 */

:- dynamic regra_acionada/2.

% =======================================================
% ORQUESTRAÇÃO PRINCIPAL
% =======================================================

% Executa todas as regras e determina o resultado
calcular_resultado :-
    % Limpa os fatos dinamicos da execucao anterior
    retractall(regra_acionada(_, _)),
    retractall(resultado_final(_)), 
    
    % Recupera/Calcula os dados iniciais
    calcular_emissao_total(EmissaoTotal),
    calcular_compensacao_total(CompensacaoTotal),
    meta_reducao(Meta),
    creditos_disponiveis(CreditosDisp),

    % Aplicar todas as regras
    aplicar_regras(EmissaoTotal, CompensacaoTotal, Meta, CreditosDisp, Resultado),
    assertz(resultado_final(Resultado)).

% =======================================================
% REGRAS DE INFERÊNCIA E CÁLCULO
% =======================================================

% REGRA 1: Calcular emissao liquida (Emissao total - Compensacao)
aplicar_regras(EmissaoTotal, CompensacaoTotal, Meta, CreditosDisp, Resultado) :-
    EmissaoLiquida is EmissaoTotal - CompensacaoTotal,
    assertz(regra_acionada(1, 'Calculo de emissao liquida: Emissao total menos compensacao (Compensacao aplicada)')),
    aplicar_meta(EmissaoTotal, EmissaoLiquida, Meta, CreditosDisp, Resultado).

% REGRA 2: Verificar se a meta foi atingida
aplicar_meta(EmissaoTotal, EmissaoLiquida, Meta, CreditosDisp, Resultado) :-
    EmissaoPermitida is EmissaoTotal * (1 - Meta),
    assertz(regra_acionada(2, 'Definicao da meta: Calculando emissao permitida com base no faturamento e meta')),
    verificar_compliance(EmissaoLiquida, EmissaoPermitida, CreditosDisp, Resultado).

% REGRA 3: Empresa esta em compliance (emissao liquida dentro da meta)
verificar_compliance(EmissaoLiquida, EmissaoPermitida, CreditosDisp, Resultado) :-
    EmissaoLiquida =< EmissaoPermitida,
    assertz(regra_acionada(3, 'Status COMPLIANT: Emissao liquida esta dentro da meta estabelecida')),
    CreditosExcedentes is EmissaoPermitida - EmissaoLiquida + CreditosDisp,
    classificar_empresa_compliant(EmissaoLiquida, EmissaoPermitida, compliant, CreditosExcedentes, CreditosDisp, Resultado).

% REGRA 4: Empresa NAO esta em compliance - precisa cobrir o deficit
verificar_compliance(EmissaoLiquida, EmissaoPermitida, CreditosDisp, Resultado) :-
    EmissaoLiquida > EmissaoPermitida,
    assertz(regra_acionada(4, 'Status NAO COMPLIANT: Emissao liquida excede a meta, necessario adquirir/usar creditos')),
    Deficit is EmissaoLiquida - EmissaoPermitida,
    avaliar_creditos(Deficit, CreditosDisp, Resultado).

% REGRA 5: Creditos existentes sao suficientes para cobrir o deficit
avaliar_creditos(Deficit, CreditosDisp, Resultado) :-
    CreditosDisp >= Deficit,
    assertz(regra_acionada(5, 'Creditos suficientes: Empresa possui creditos disponiveis para cobrir o deficit')),
    CreditosRestantes is CreditosDisp - Deficit,
    classificar_empresa_compliant(0, Deficit, compliant_com_creditos, CreditosRestantes, CreditosDisp, Resultado). % Usamos 0 e Deficit como base para a classificacao

% REGRA 6: Creditos insuficientes - precisa comprar mais
avaliar_creditos(Deficit, CreditosDisp, Resultado) :-
    CreditosDisp < Deficit,
    CreditosNecessarios is Deficit - CreditosDisp,
    assertz(regra_acionada(6, 'Creditos insuficientes: Necessario adquirir creditos adicionais no mercado')),
    calcular_investimento(CreditosNecessarios, Investimento),
    classificar_empresa_nao_compliant(CreditosNecessarios, Investimento, CreditosDisp, Resultado).


% =======================================================
% REGRA 7: CLASSIFICAÇÃO DE RISCO (COMPLIANT e COM CRÉDITOS)
% =======================================================

% Classificacao para empresas em COMPLIANCE ou COMPLIANCE_COM_CREDITOS
classificar_empresa_compliant(EmissaoLiquida, EmissaoPermitida, Status, CreditosExcedentes, CreditosDisp, Resultado) :-
    (   Status == compliant -> BaseComparacao is EmissaoLiquida
    ;   Status == compliant_com_creditos -> BaseComparacao is 0 % Zero porque o deficit foi coberto
    ),
    
    (   BaseComparacao =< EmissaoPermitida * 0.5
    ->  Classificacao = 'Excelente - Baixa emissao/Alto superavit',
        assertz(regra_acionada('7.1', 'Classificacao EXCELENTE: Emissoes muito abaixo da meta'))
    ;   BaseComparacao =< EmissaoPermitida * 0.8
    ->  Classificacao = 'Bom - Emissao moderada',
        assertz(regra_acionada('7.2', 'Classificacao BOA: Emissoes controladas dentro da meta'))
    ;   Classificacao = 'Regular - Proximo ao limite',
        assertz(regra_acionada('7.3', 'Classificacao REGULAR: Emissoes proximas ao limite da meta'))
    ),
    recomendar_projeto(Status, Recomendacao),
    Resultado = _{
        status: Status,
        classificacao: Classificacao,
        creditos_necessarios: 0.0,
        creditos_excedentes: CreditosExcedentes,
        investimento: 0.0,
        recomendacao: Recomendacao,
        creditos_disponiveis: CreditosDisp
    }.

% Sobrecarga para o caso NAO COMPLIANT (Regra 7.4, 7.5, 7.6)
classificar_empresa_nao_compliant(CreditosNecessarios, Investimento, CreditosDisp, Resultado) :-
    (   CreditosNecessarios < 100
    ->  Classificacao = 'Critico - Deficit baixo',
        assertz(regra_acionada('7.4', 'Classificacao CRITICA com deficit baixo (necessidade < 100 tCO2e)'))
    ;   CreditosNecessarios < 500
    ->  Classificacao = 'Critico - Deficit moderado',
        assertz(regra_acionada('7.5', 'Classificacao CRITICA com deficit moderado (necessidade 100-500 tCO2e)'))
    ;   Classificacao = 'Critico - Deficit alto',
        assertz(regra_acionada('7.6', 'Classificacao CRITICA com deficit alto (necessidade > 500 tCO2e)'))
    ),
    recomendar_projeto(nao_compliant, Recomendacao),
    Resultado = _{
        status: nao_compliant,
        classificacao: Classificacao,
        creditos_necessarios: CreditosNecessarios,
        creditos_excedentes: 0.0,
        investimento: Investimento,
        recomendacao: Recomendacao,
        creditos_disponiveis: CreditosDisp
    }.

% REGRA 8: Calcular investimento necessario
calcular_investimento(CreditosNecessarios, Investimento) :-
    preco_credito(Preco), % Assume-se que preco_credito/1 esta definido em outro arquivo (ex: knowledge.pl)
    Investimento is CreditosNecessarios * Preco,
    assertz(regra_acionada(8, 'Calculo de investimento: Multiplicacao de creditos necessarios pelo preco unitario')).

% REGRA 9: Recomendar tipo de projeto baseado no status
recomendar_projeto(compliant, 'reflorestamento (para gerar mais creditos)') :-
    assertz(regra_acionada(9, 'Recomendacao: Reflorestamento para empresas em compliance gerarem creditos extras')).

recomendar_projeto(compliant_com_creditos, 'eficiencia energetica (para reduzir custos e dependencia de creditos)') :-
    assertz(regra_acionada(9, 'Recomendacao: Eficiencia energetica para otimizar operacoes')).

recomendar_projeto(nao_compliant, 'energia renovavel e compra de creditos urgente') :-
    assertz(regra_acionada(9, 'Recomendacao: Energia renovavel e aquisicao urgente de creditos para compliance')).