describe('Teste de editar um edificio', () => {
  
    beforeEach(() => {
        //criar edificio para editar
        cy.visit('http://localhost:4200/login');
        cy.get('#login-input-username').type('admin@email.pt');
        cy.get('#login-input-password').type('admin');
        cy.get('button[type="submit"]').click();
        cy.get('[id^="Campus"]').click();
        cy.get('[routerlink="create-building"]').click();
        const numeroAleatorio = Math.floor(1000 + Math.random() * 9000);
        cy.get('#mat-input-9').type('A'+numeroAleatorio.toString());
        cy.get('#mat-input-10').type("10");
        cy.get('#mat-input-11').type('10');
        cy.get('#mat-input-12').type('teste');
        cy.get('#mat-input-13').type('Mais um edificio de teste');
        cy.get('.mdc-button__label').click();
        //ir para a pagina correta do teste
        cy.get('[id^="Campus"]').click();
        cy.get('[routerlink="edit-building"]').click();
    });
  
    
    it('Devem retornar 200 depois de fazer o patch.', () => {
        cy.intercept('PATCH', 'http://localhost:4000/api/buildings').as('rotaBuilding');

        cy.get('#buildingSelector').click().get('#mat-option-0').click();
        cy.get('#mat-input-15').clear().type('20');
        cy.get('#mat-input-15').clear().type('20');
        cy.get('#saveBuilding').click();

        cy.wait('@rotaBuilding').should(({ response }) => {
            expect(response!.statusCode).to.eq(200); 
        });
    });

  
  });