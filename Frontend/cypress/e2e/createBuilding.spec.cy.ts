describe('Teste de criar um edificio', () => {
  
    beforeEach(() => {
        cy.visit('http://localhost:4200/login');
        cy.get('#role').select('Administrador');
        cy.get('#signUp').click();
        cy.get('[id^="Campus"]').click();
        cy.get('[routerlink="create-building"]').click();
    });
  
    
    it('Devem existir todos os campos.', () => {

        cy.get('#mat-input-0').should('exist');
        cy.get('#mat-input-1').should('exist');
        cy.get('#mat-input-2').should('exist');
        cy.get('#mat-input-3').should('exist');
        cy.get('#mat-input-4').should('exist');
        
    });
    it('Todos os campos devem estar vazios.', () => {

        cy.get('#mat-input-0').should('have.value', '');
        cy.get('#mat-input-1').should('have.value', '');
        cy.get('#mat-input-2').should('have.value', '');
        cy.get('#mat-input-3').should('have.value', '');
        cy.get('#mat-input-4').should('have.value', '');
        
    });

    it('Deve ser possivel criar edificio e receber mensagem de sucesso.', () => {
        //rota a interceptar para receber o codigo de sucesso
        cy.intercept('POST', 'http://localhost:4000/api/buildings').as('rotaBuilding');

        //numero aleaorio para codigo de edificio
        const numeroAleatorio = Math.floor(1000 + Math.random() * 9000);
        cy.get('#mat-input-0').type('A'+numeroAleatorio.toString());
        cy.get('#mat-input-1').type("10");
        cy.get('#mat-input-2').type('10');
        cy.get('#mat-input-3').type('teste');
        cy.get('#mat-input-4').type('Mais um edificio de teste');
        cy.get('.mdc-button__label').click();
        cy.get('.mat-mdc-simple-snack-bar > .mat-mdc-snack-bar-label').should('be.visible');

        cy.wait('@rotaBuilding').should(({ response }) => {
            expect(response!.statusCode).to.eq(200); 
        });
        
    });
  
  });