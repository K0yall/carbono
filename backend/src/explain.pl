/*
 * explain.pl - Explicacao do Raciocinio
 * Mostra trilha de regras acionadas com contexto numerico.
 */

% Para acessar os fatos dinamicos
:- dynamic regra_acionada/2. 
:- dynamic emissao_total/1.
:- dynamic meta_reducao/1.
:- dynamic resultado_final/1.

% Explica como o sistema chegou ao resultado
explicar_resultado :-
    nl,
    writeln('========================================================'),
    writeln('             EXPLICACAO DO RACIOCINIO                   '),
    writeln('========================================================'),
    nl,

    % 1. DADOS CHAVE DE CONTEXTO
    writeln('### 1. DADOS E METAS CALCULADAS ###'),
    nl,
    
    (emissao_total(EmTotal) ->
        format('   - Emissao Total Bruta: ............... ~2f tCO2e~n', [EmTotal])
    ; true),

    (meta_reducao(MetaDecimal), emissao_total(EmTotal) ->
        EmissaoPermitida is EmTotal * (1 - MetaDecimal),
        MetaPercentual is MetaDecimal * 100,
        format('   - Meta de Reducao (Aplicada): ........ ~0f%%~n', [MetaPercentual]),
        format('   - Emissao Maxima Permitida (Target): ~2f tCO2e~n', [EmissaoPermitida])
    ; true),

    nl,
    writeln('========================================================'),
    
    % 2. RESUMO DE CREDITOS
    (resultado_final(R) ->
        CreditosNec = R.creditos_necessarios,
        CreditosExc = R.creditos_excedentes,
        
        nl,
        writeln('### 2. RESUMO DE CREDITOS E INVESTIMENTO ###'),
        nl,
        
        (CreditosNec > 0 ->
            format('   >> DEFICIT ENCONTRADO (Compra Necessaria): ~2f tCO2e~n', [CreditosNec]),
            format('   >> Investimento Estimado para COBRIR: ...... R$ ~2f~n', [R.investimento])
        ; CreditosExc > 0 ->
            format('   >> EXCEDENTE ENCONTRADO (Para Venda): ..... ~2f tCO2e~n', [CreditosExc])
        ;
            writeln('   >> COMPLIANCE ALCANCADO com saldo nulo ou muito baixo.')
        )
    ; true),

    nl,
    writeln('========================================================'),
    
    % 3. TRILHA DE REGRAS
    nl,
    writeln('### 3. TRILHA DE REGRAS DE INFERENCIA ###'),
    nl,
    
    findall(Num-Explicacao, regra_acionada(Num, Explicacao), Regras),
    sort(Regras, RegrasOrdenadas), % Ordena por numero de regra (Num)
    exibir_regras(RegrasOrdenadas),

    nl,
    writeln('========================================================').


% Exibe cada regra acionada com melhor formatacao
exibir_regras([]).
exibir_regras([Num-Explicacao|Resto]) :-
    format(' [~w] - ~w~n', [Num, Explicacao]),
    exibir_regras(Resto).