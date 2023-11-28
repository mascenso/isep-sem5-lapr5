describe('Teste de criar um Robot Type', () => {
  
    beforeEach(() => {
        cy.visit('http://localhost:4200/login');
        cy.get('#role').select('Administrador');
        cy.get('#signUp').click();
        cy.get('[id^="Fleet"]').click();
        cy.get('[routerlink="create-robot-type"]').click();
    });
  
    
    it('Devem existir todos os campos.', () => {

        cy.get('#mat-input-0').should('exist');
        cy.get('#mat-select-0').should('exist');
    });
    it('O campo de designação deve estar vazio.', () => {

        cy.get('#mat-input-0').should('have.value', '');
    });
    it('A seleção de Task Types deve ter valores.', () => {

        cy.get('#mat-select-0').should!('have.value', '');
    });
    it('A seleção de Task Types deve ter a task Segurança.', () => {

        cy.get('#mat-select-0').click();  // Click on the dropdown to open it
        cy.get('mat-option').contains('Segurança').click();  // Assuming 'A9426' is an option in the dropdown
    });
    it('A seleção de Task Types deve ter a task Entregas.', () => {

        cy.get('#mat-select-0').click();  // Click on the dropdown to open it
        cy.get('mat-option').contains('Entregas').click();  // Assuming 'A9426' is an option in the dropdown
    });
    it('A seleção de Task Types deve ter a task Outras.', () => {

        cy.get('#mat-select-0').click();  // Click on the dropdown to open it
        cy.get('mat-option').contains('Outras').click();  // Assuming 'A9426' is an option in the dropdown
    });

    it('Deve ser possivel criar robot type e receber mensagem de sucesso.', () => {
        //rota a interceptar para receber o codigo de sucesso
        cy.intercept('POST', 'http://localhost:4000/api/robots/types').as('rotaRobotType');

        //numero aleaorio para codigo de edificio
        const numeroAleatorio = Math.floor(1000 + Math.random() * 9000);
        cy.get('#mat-input-0').type('A'+numeroAleatorio);
        cy.get('#mat-select-0').click();  
        cy.get('mat-option').contains('Outras').click(); 
        cy.get('.mdc-button__label').click({multiple: true, force: true});
        cy.get('.mat-mdc-simple-snack-bar > .mat-mdc-snack-bar-label').should('be.visible');
        cy.wait('@rotaRobotType').should(({ response }) => {
            expect(response!.statusCode).to.eq(200); 
        });
        
    });
  
  });