class CarbonoSystem {
    constructor() {
        this.apiUrl = 'http://localhost:8000/api/consultar';
        this.initEventListeners();
    }

    initEventListeners() {
        // Form submission
        document.getElementById('carbono-form').addEventListener('submit', (e) => {
            e.preventDefault();
            this.calcularAnalise();
        });

        // Toggle compensação details
        document.getElementById('tem-compensacao').addEventListener('change', (e) => {
            const detalhes = document.getElementById('compensacao-detalhes');
            detalhes.style.display = e.target.value === 'sim' ? 'block' : 'none';
        });
    }

    async calcularAnalise() {
        const btn = document.getElementById('calcular-btn');
        const loading = document.getElementById('loading');

        try {
            // Show loading
            btn.disabled = true;
            loading.style.display = 'block';

            // Get form data
            const formData = this.getFormData();

            // Send to backend
            const resultado = await this.sendToBackend(formData);

            // Display results
            this.displayResults(resultado);

        } catch (error) {
            this.showError('Erro ao processar consulta: ' + error.message);
        } finally {
            // Hide loading
            btn.disabled = false;
            loading.style.display = 'none';
        }
    }

    getFormData() {
        const temCompensacao = document.getElementById('tem-compensacao').value;

        return {
            setores: [
                { nome: 'energia', faturamento: parseFloat(document.getElementById('energia').value) || 0 },
                { nome: 'transportes', faturamento: parseFloat(document.getElementById('transportes').value) || 0 },
                { nome: 'industria', faturamento: parseFloat(document.getElementById('industria').value) || 0 },
                { nome: 'agropecuaria', faturamento: parseFloat(document.getElementById('agropecuaria').value) || 0 },
                { nome: 'residuos', faturamento: parseFloat(document.getElementById('residuos').value) || 0 }
            ],
            meta: (parseFloat(document.getElementById('meta').value) || 0) / 100,
            compensacao: {
                tipo: temCompensacao === 'sim' ? document.getElementById('tipo-compensacao').value : 'nenhum',
                quantidade: temCompensacao === 'sim' ? parseFloat(document.getElementById('quantidade-compensacao').value) || 0 : 0
            },
            creditos_existentes: parseFloat(document.getElementById('creditos-existentes').value) || 0
        };
    }

    async sendToBackend(data) {
        const response = await fetch(this.apiUrl, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify(data)
        });

        if (!response.ok) {
            throw new Error(`HTTP error! status: ${response.status}`);
        }

        return await response.json();
    }

    displayResults(resultado) {
        if (resultado.error) {
            this.showError(resultado.error);
            return;
        }

        // Show resultado section
        document.getElementById('resultado-section').style.display = 'block';
        document.getElementById('explicacao-section').style.display = 'block';

        // Display main results
        this.displayMainResults(resultado);

        // Display explanation
        this.displayExplanation(resultado.explicacao);
    }

    displayMainResults(resultado) {
        const content = document.getElementById('resultado-content');
        const r = resultado.resultado;

        let html = `
            <div class="resultado-item">
                <strong>Emissão Total:</strong> ${resultado.emissao_total.toFixed(2)} tCO2e
            </div>
            <div class="resultado-item">
                <strong>Compensação Total:</strong> ${resultado.compensacao_total.toFixed(2)} tCO2e
            </div>
            <div class="resultado-item ${r.status === 'compliant' ? 'status-compliant' : 'status-non-compliant'}">
                <strong>Status Compliance:</strong> ${r.status === 'compliant' ? '✅ EM COMPLIANCE' : '❌ NÃO COMPLIANT'}
            </div>
            <div class="resultado-item">
                <strong>Classificação:</strong> ${r.classificacao}
            </div>
        `;

        if (r.creditos_necessarios > 0) {
            html += `
                <div class="resultado-item status-non-compliant">
                    <strong>Créditos Necessários:</strong> ${r.creditos_necessarios} tCO2e
                </div>
                <div class="resultado-item">
                    <strong>Investimento Estimado:</strong> R$ ${r.investimento.toFixed(2)}
                </div>
            `;
        } else {
            html += `
                <div class="resultado-item status-compliant">
                    <strong>✅ SEM NECESSIDADE DE COMPRA DE CRÉDITOS</strong>
                </div>
            `;
        }

        if (r.creditos_excedentes > 0) {
            html += `
                <div class="resultado-item status-compliant">
                    <strong>Créditos Excedentes:</strong> ${r.creditos_excedentes} tCO2e (para venda)
                </div>
            `;
        }

        html += `
            <div class="recomendacao">
                <strong>📋 RECOMENDAÇÃO:</strong> Investir em projeto de ${r.recomendacao}
            </div>
        `;

        content.innerHTML = html;
    }

    displayExplanation(explicacao) {
        const content = document.getElementById('explicacao-content');

        if (!explicacao || explicacao.length === 0) {
            content.innerHTML = '<p>Nenhuma regra foi acionada durante a análise.</p>';
            return;
        }

        let html = '<h4>Regras aplicadas durante a análise:</h4>';
        explicacao.forEach((regra, index) => {
            html += `
                <div class="explicacao-item">
                    <strong>${index + 1}.</strong> ${regra}
                </div>
            `;
        });

        content.innerHTML = html;
    }

    showError(message) {
        const content = document.getElementById('resultado-content');
        content.innerHTML = `
            <div class="resultado-item status-non-compliant">
                <strong>Erro:</strong> ${message}
            </div>
        `;
        document.getElementById('resultado-section').style.display = 'block';
        document.getElementById('explicacao-section').style.display = 'none';
    }
}

// Initialize the application when DOM is loaded
document.addEventListener('DOMContentLoaded', () => {
    new CarbonoSystem();
});