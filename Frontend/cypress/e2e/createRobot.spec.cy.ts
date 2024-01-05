describe('Teste de criar um Robot', () => {
  
    beforeEach(() => {
        cy.visit('http://localhost:4200/login');
        cy.get('#login-input-username').type('admin@email.pt');
        cy.get('#login-input-password').type('admin');
        cy.get('button[type="submit"]').click();
        cy.get('[id^="Fleet"]').click();
        cy.get('[routerlink="create-robot"]').click();
    });
  
    
    it('Devem existir todos os campos.', () => {

        cy.get('#mat-input-9').should('exist');
        cy.get('#dropdownSelectRobotType').should('exist');
        cy.get('#mat-input-10').should('exist');
        cy.get('#mat-input-11').should('exist');

        
    });
    it('Todos os campos devem estar vazios.', () => {

        cy.get('#mat-input-9').should('have.value', '');
        cy.get('#dropdownSelectRobotType').should('have.value', '');
        cy.get('#mat-input-10').should('have.value', '');
        cy.get('#mat-input-11').should('have.value', '');

    });

    it('Deve ser possivel criar robots e receber mensagem de sucesso.', () => {
        //rota a interceptar para receber o codigo de sucesso
        cy.intercept('POST', 'http://localhost:4000/api/robots').as('rotaRobots');

        const numeroAleatorio = Math.floor(1000 + Math.random() * 90000000);
        cy.get('#mat-input-9').type('X'+numeroAleatorio+'1');
        cy.get('#dropdownSelectRobotType').click();
        cy.get('mat-option').contains('TipoX - Segurança,Entregas').click();
        cy.get('#mat-input-10').type('1'+numeroAleatorio);
        cy.get('#mat-input-11').type('Teste');
        cy.get('.mdc-button__label').click({multiple: true, force: true});
        cy.get('.mat-mdc-simple-snack-bar > .mat-mdc-snack-bar-label').should('be.visible');

        cy.wait('@rotaRobots').should(({ response }) => {
            expect(response!.statusCode).to.eq(200); 
        });
        
    });


  
  });