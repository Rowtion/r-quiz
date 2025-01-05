class Quiz {
    constructor() {
        this.score = 0;
        this.nickname = '';
        this.questions = [
            {
                category: 'R基础与包管理',
                difficulty: 'easy',
                question: '在R中，以下哪个包集合最适合用于数据处理和可视化？',
                code: `# 示例代码
library(tidyverse)
library(ggplot2)
library(dplyr)`,
                options: [
                    'base R + stats',
                    'tidyverse + ggplot2',
                    'data.table + lattice',
                    'reshape2 + plotly'
                ],
                correct: 1,
                explanation: 'tidyverse是一个包集合，它包含了数据处理(dplyr)、可视化(ggplot2)等多个强大的包，是现代R编程的标准工具集。'
            },
            {
                category: 'TCGA数据分析',
                difficulty: 'medium',
                question: '在处理TCGA数据时，将counts数据转换为TPM的正确步骤是什么？',
                code: `# 转换步骤示例
counts2fpkm <- function(counts, gene_length) {
    N <- sum(counts)
    return(counts * 1e9 / gene_length / N)
}

fpkm2tpm <- function(fpkm) {
    return(fpkm * 1e6 / sum(fpkm))
}`,
                options: [
                    '直接将counts除以总reads数',
                    '先转换为RPKM，再标准化',
                    '先转换为FPKM，再转换为TPM',
                    '直接将counts乘以一个常数'
                ],
                correct: 2,
                explanation: '正确的转换流程是：先考虑基因长度和测序深度转换为FPKM，然后将FPKM标准化为TPM。这样可以消除基因长度和测序深度的影响。'
            },
            {
                category: '数据可视化',
                difficulty: 'medium',
                question: '使用ggplot2创建复杂可视化图形时，下列哪个语句是正确的？',
                code: `# 示例代码
ggplot(climate_data, 
       aes(x = temperature, 
           y = rainfall, 
           size = year, 
           color = city)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis(discrete = TRUE)`,
                options: [
                    'geom_point()必须在aes()之前',
                    'color必须在geom_point()中设置',
                    '可以用+号添加多个图层',
                    'scale_color_viridis()不能用于离散变量'
                ],
                correct: 2,
                explanation: 'ggplot2基于图形语法，使用+号逐层添加图形元素是其核心特征。每个图层都可以独立设置属性，并且按照添加顺序绘制。'
            },
            {
                category: 'GEO数据分析',
                difficulty: 'hard',
                question: '在处理来自不同GEO数据集的数据时，如何正确处理批次效应？',
                code: `# 批次效应处理示例
library(sva)
batch <- c(rep(1, ncol(data1)), rep(2, ncol(data2)))
adjusted_matrix <- ComBat(
    dat = cbind(data1, data2),
    batch = batch
)`,
                options: [
                    '直接合并数据集即可',
                    '使用ComBat进行批次效应校正',
                    '分别对每个数据集进行标准化',
                    '删除批次效应明显的样本'
                ],
                correct: 1,
                explanation: 'ComBat是一种广泛使用的批次效应校正方法，它可以有效去除非生物学变异，同时保留生物学信号。在合并多个数据集时，批次效应校正是必要的步骤。'
            },
            {
                category: '数据预处理',
                difficulty: 'hard',
                question: '在进行差异表达分析之前，需要进行哪些必要的数据预处理步骤？',
                code: `# 数据预处理示例
# 1. 数据标准化
normalized_data <- normalizeBetweenArrays(raw_data)

# 2. 质量控制
boxplot(normalized_data)
hist(normalized_data)

# 3. 批次效应检查
pca_plot <- plotPCA(normalized_data)`,
                options: [
                    '仅需要标准化数据',
                    '标准化和质量控制',
                    '标准化、质量控制和批次效应检查',
                    '直接进行差异分析'
                ],
                correct: 2,
                explanation: '完整的预处理流程应包括：数据标准化、质量控制和批次效应检查。这些步骤能确保后续分析的可靠性。'
            },
            {
                category: '差异表达分析',
                difficulty: 'medium',
                question: '在使用limma包进行差异表达分析时，以下哪个步骤是正确的？',
                code: `# 差异分析示例
design <- model.matrix(~0 + group)
colnames(design) <- levels(group)
fit <- lmFit(expr_matrix, design)
contrast.matrix <- makeContrasts(
    AD-Normal,
    levels = design
)
fit2 <- contrasts.fit(fit, contrast.matrix)
fit2 <- eBayes(fit2)
DEGs <- topTable(fit2, coef = 1, n = Inf)`,
                options: [
                    '直接使用t检验比较两组',
                    '不需要设计矩阵，直接用eBayes',
                    '先构建设计矩阵，再用lmFit和eBayes',
                    '只需要用wilcox.test就够了'
                ],
                correct: 2,
                explanation: 'limma包的差异分析流程需要：1)构建设计矩阵；2)使用lmFit拟合线性模型；3)使用contrasts.fit设置对比；4)使用eBayes进行贝叶斯估计。这样可以得到更稳健的结果。'
            },
            {
                category: '差异表达分析',
                difficulty: 'hard',
                question: '在筛选差异表达基因时，通常使用什么标准？',
                code: `# 差异基因筛选示例
DEGs <- topTable(fit2, coef = 1, n = Inf) %>%
    filter(abs(logFC) > 1 & adj.P.Val < 0.05)

# 添加上下调标记
DEGs <- DEGs %>%
    mutate(change = case_when(
        logFC > 1 & adj.P.Val < 0.05 ~ "Up",
        logFC < -1 & adj.P.Val < 0.05 ~ "Down",
        TRUE ~ "NS"
    ))`,
                options: [
                    '只看p值小于0.05',
                    '只看fold change大于2',
                    'p值小于0.05且|logFC|大于1',
                    '随便选择一些基因就行'
                ],
                correct: 2,
                explanation: '差异基因的筛选通常需要同时考虑统计显著性(p值或校正后的p值)和生物学意义(fold change)。通常的标准是adj.P.Val < 0.05且|logFC| > 1，这样可以确保筛选出的基因既具有统计显著性，又有足够大的表达差异。'
            },
            {
                category: '功能富集分析',
                difficulty: 'medium',
                question: '使用clusterProfiler进行GO富集分析时，enrichGO函数的关键参数是什么？',
                code: `# GO富集分析示例
ego <- enrichGO(
    gene = gene_list,
    OrgDb = org.Hs.eg.db,
    keyType = "ENTREZID",
    ont = "BP",
    pAdjustMethod = "BH",
    pvalueCutoff = 0.05,
    qvalueCutoff = 0.05
)`,
                options: [
                    '只需要提供基因列表',
                    '需要基因列表和物种数据库',
                    '基因列表、物种数据库和ID类型都需要',
                    '不需要设置任何参数'
                ],
                correct: 2,
                explanation: 'enrichGO函数需要设置多个关键参数：1)gene：基因列表；2)OrgDb：物种注释数据库；3)keyType：基因ID类型；4)ont：GO分类(BP/MF/CC)；5)pAdjustMethod：p值校正方法。这些参数都很重要，确保富集分析的准确性。'
            },
            {
                category: '功能富集分析',
                difficulty: 'hard',
                question: '在进行KEGG富集分析之前，为什么需要进行基因ID转换？',
                code: `# 基因ID转换示例
gene_list <- bitr(
    gene = gene_list,
    fromType = "SYMBOL",
    toType = "ENTREZID",
    OrgDb = org.Hs.eg.db
)

# KEGG富集分析
ekegg <- enrichKEGG(
    gene = gene_list$ENTREZID,
    organism = "hsa",
    pvalueCutoff = 0.05
)`,
                options: [
                    '不需要转换，直接用基因名就可以',
                    'KEGG数据库只认ENTREZ ID',
                    '转换是可选的，不影响结果',
                    '只是为了好看'
                ],
                correct: 1,
                explanation: 'KEGG数据库使用ENTREZ ID作为基因标识符。如果不进行转换，将无法正确进行富集分析。使用bitr函数可以将基因符号(SYMBOL)转换为ENTREZ ID，这是KEGG富集分析的必要步骤。'
            },
            {
                category: '结果可视化',
                difficulty: 'medium',
                question: '在可视化差异分析结果时，火山图中的哪些信息最重要？',
                code: `# 火山图绘制示例
ggplot(DEGs, aes(x = logFC, y = -log10(adj.P.Val))) +
    geom_point(aes(color = change)) +
    geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
    geom_vline(xintercept = c(-1, 1), linetype = "dashed") +
    scale_color_manual(values = c("Up" = "red", "Down" = "blue", "NS" = "grey")) +
    theme_bw()`,
                options: [
                    '只需要展示p值',
                    '只需要展示fold change',
                    '需要同时展示p值和fold change',
                    '随便画点就行'
                ],
                correct: 2,
                explanation: '火山图是展示差异分析结果的经典方式，它同时展示了：1)logFC：反映表达变化程度；2)-log10(p值)：反映统计显著性；3)通过虚线标记筛选阈值；4)用不同颜色标记上下调基因。这样可以直观地展示差异基因的分布情况。'
            },
            {
                category: '功能富集分析',
                difficulty: 'hard',
                question: '如何解释富集分析的结果？',
                code: `# 富集分析结果展示
enrichResult %>%
    as.data.frame() %>%
    arrange(p.adjust) %>%
    mutate(
        GeneRatio = parse_ratio(GeneRatio),
        richFactor = Count / as.numeric(sub("/\\d+", "", BgRatio))
    ) %>%
    head(10)`,
                options: [
                    '只看p值最小的通路',
                    '只关注基因数量最多的通路',
                    '综合考虑p值、富集因子和基因数量',
                    '随便选几个通路'
                ],
                correct: 2,
                explanation: '解释富集分析结果需要综合考虑多个指标：1)p.adjust：校正后的显著性；2)richFactor：富集程度；3)Count：富集基因数量；4)GeneRatio：基因比例。这样可以更全面地理解富集通路的生物学意义。'
            },
            {
                category: '生物信息学可视化',
                difficulty: 'medium',
                question: '在使用ComplexHeatmap绘制热图时，如何处理基因表达值的标准化？',
                code: `# 热图数据预处理
sig_matrix_scaled <- t(scale(t(sig_matrix)))
col_fun <- colorRamp2(
    breaks = c(min(sig_matrix_scaled), 0, max(sig_matrix_scaled)),
    colors = c("#1f77b4", "white", "#e31a1c")
)`,
                options: [
                    '不需要标准化',
                    '只需要log2转换',
                    '需要进行Z-score标准化',
                    '直接用原始值'
                ],
                correct: 2,
                explanation: '在绘制热图时，通常需要对基因表达值进行Z-score标准化，这样可以：1)消除基因间表达量级的差异；2)突出表达模式的差异；3)使颜色区分更加明显。scale函数可以对每行数据进行标准化处理。'
            }
        ];
        this.initializeQuiz();
    }

    initializeQuiz() {
        // 添加昵称输入
        const nicknameInput = prompt('请输入你的学员昵称：');
        this.nickname = nicknameInput || '匿名学员';
        
        this.renderAllQuestions();
        document.getElementById('submit-button').addEventListener('click', () => this.submitQuiz());
    }

    renderAllQuestions() {
        const questionContainer = document.getElementById('question-container');
        
        questionContainer.innerHTML = this.questions.map((question, index) => `
            <div class="question-container" data-question-index="${index}">
                <div class="question-header">
                    <span class="category">${question.category}</span>
                    <span class="question-difficulty difficulty-${question.difficulty}">${question.difficulty.toUpperCase()}</span>
                </div>
                <h3>${index + 1}. ${question.question}</h3>
                <div class="code-block">
                    <pre><code>${question.code}</code></pre>
                </div>
                <div class="option-container">
                    ${question.options.map((option, optIndex) => `
                        <button class="option-button" data-index="${optIndex}">
                            ${option}
                        </button>
                    `).join('')}
                </div>
                <div class="explanation" id="explanation-${index}">
                    ${question.explanation}
                </div>
            </div>
        `).join('');

        // Add event listeners to options
        document.querySelectorAll('.option-button').forEach(button => {
            button.addEventListener('click', (e) => this.selectOption(e.target));
        });
    }

    selectOption(button) {
        // Remove selected class from all options in the same question
        const questionContainer = button.closest('.question-container');
        questionContainer.querySelectorAll('.option-button').forEach(btn => {
            btn.classList.remove('selected');
        });
        // Add selected class to clicked option
        button.classList.add('selected');
    }

    submitQuiz() {
        this.score = 0;
        
        // Check answers for all questions
        this.questions.forEach((question, index) => {
            const questionContainer = document.querySelector(`[data-question-index="${index}"]`);
            const selectedOption = questionContainer.querySelector('.option-button.selected');
            
            if (selectedOption) {
                const selectedIndex = parseInt(selectedOption.dataset.index);
                if (selectedIndex === question.correct) {
                    this.score++;
                    selectedOption.classList.add('correct');
                } else {
                    selectedOption.classList.add('incorrect');
                    // Show correct answer
                    const correctOption = questionContainer.querySelector(`[data-index="${question.correct}"]`);
                    correctOption.classList.add('correct');
                }
            }
            
            // Show explanation
            document.getElementById(`explanation-${index}`).classList.add('visible');
        });

        this.showResults();
    }

    showResults() {
        const percentage = (this.score / this.questions.length) * 100;
        const resultsContainer = document.getElementById('results-container');
        resultsContainer.style.display = 'block';
        
        // 生成答题详情
        const questionResults = this.questions.map((question, index) => {
            const selectedAnswer = document.querySelector(`.question-container[data-question-index="${index}"] .option-button.selected`);
            const isCorrect = selectedAnswer && parseInt(selectedAnswer.dataset.index) === question.correct;
            return `
                <div class="question-result ${isCorrect ? 'correct' : 'incorrect'}">
                    <span class="question-number">题目 ${index + 1}</span>
                    <span class="result-icon">${isCorrect ? '✓' : '✗'}</span>
                    <span class="question-category">${question.category}</span>
                </div>
            `;
        }).join('');

        resultsContainer.innerHTML = `
            <h2>测验结果</h2>
            <div class="score-display">[${this.nickname}]得分: ${this.score}/${this.questions.length} (${percentage.toFixed(1)}%)</div>
            <div class="feedback-container">
                ${this.generateFeedback(percentage)}
            </div>
            <div class="questions-summary">
                <h3>答题详情</h3>
                <div class="questions-grid">
                    ${questionResults}
                </div>
            </div>
            <div id="screenshot-reminder" class="reminder-box">
                <h3>🎉 恭喜完成测验！</h3>
                <p>请记得截图打卡:</p>
                <ol>
                    <li>📸 截图保存您的测验成绩</li>
                    <li>📝 请提交成绩截图至飞书作业打卡文档</li>
                </ol>
            </div>
        `;
        
        // Disable all option buttons
        document.querySelectorAll('.option-button').forEach(btn => {
            btn.disabled = true;
        });
        
        // Hide submit button
        document.getElementById('submit-button').style.display = 'none';
    }

    generateFeedback(percentage) {
        if (percentage >= 90) {
            return '太棒了！你对R语言和生物信息学分析有很深的理解。';
        } else if (percentage >= 70) {
            return '不错的表现！但还有一些概念需要加强。';
        } else if (percentage >= 50) {
            return '及格了，但需要更多练习来提高。';
        } else {
            return '建议重新复习课程内容，特别是基础概念。';
        }
    }
}

// 页面加载完成后初始化测验
document.addEventListener('DOMContentLoaded', () => {
    new Quiz();
});
