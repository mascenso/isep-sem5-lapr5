describe('Teste de criar um Floor', () => {
  
    beforeEach(() => {
        cy.visit('http://localhost:4200/login');
        cy.get('#login-input-username').type('admin@email.pt');
        cy.get('#login-input-password').type('admin');
        cy.get('button[type="submit"]').click();
        cy.get('[id^="Campus"]').click();
        cy.get('[routerlink="create-floor"]').click();
    });
  
    
    it('Devem existir todos os campos.', () => {

        cy.get('#mat-input-9').should('exist');
        cy.get('#mat-input-10').should('exist');
        cy.get('#mat-input-11').should('exist');
        cy.get('#mat-input-12').should('exist');

        
    });
    it('Todos os campos devem estar vazios.', () => {

        cy.get('#mat-input-9').should('have.value', '');
        cy.get('#mat-input-10').should('have.value', '');
        cy.get('#mat-input-11').should('have.value', '');
        cy.get('#mat-input-12').should('have.value', '');

    });

    it('Deve ser possivel criar floor e receber mensagem de sucesso.', () => {
        //rota a interceptar para receber o codigo de sucesso
        cy.intercept('POST', 'http://localhost:4000/api/floors').as('rotaFloor');

        cy.get('#dropdownCreateFloor').click().get('#mat-option-0').click();

        cy.get('#mat-input-9').type("10");
        cy.get('#mat-input-10').type('10');
        cy.get('#mat-input-11').type('4');
        cy.get('#mat-input-12').type('Mais um floor de teste');
        cy.get('#saveNewFloor').click();

        cy.wait('@rotaFloor').should(({ response }) => {
            expect(response!.statusCode).to.eq(200); 
        });
        
    });


  
  });